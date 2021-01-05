{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module RekordBuddy.Printer.Body where

import           Data.Text (Text)
import           Data.Text.Prettyprint.Doc
import           Data.Maybe(fromMaybe, mapMaybe, fromJust)
import qualified Data.Map.Lazy as Map
import           Data.List (sort, nub)
import           RekordBuddy.Types
import           RekordBuddy.Helpers
import qualified RekordBuddy.Configuration as Con
import           RekordBuddy.Printer.NxA

bodyDoc :: Con.Configuration -> Entity -> Doc ann
bodyDoc config e =
   doubleLined (copyright : prologue : attributes : map (relationshipDef config e) relns)
 where
   relns = Map.elems $ entityRelationships e

   attributes = doubleLined (map (attributeDef config e) attrs)

   ptypeInternal Nothing      = "PersistentObject"
   ptypeInternal (Just a)     = internalType a

   faultRelnDoc r | relationshipToMany r =
         optionalCond config r instanceName (\name -> [callS (name /->/ "faultRelationship") []])
   faultRelnDoc _ = mempty

   cascadeDeleteDoc r | relationshipToMany r =
      let inv = fromJust (inverseRelationship r (Con.configurationEntities config))
       in optionalCond config r instanceName
            (\name ->
               [ autoForP (Con.nesting config) "inverse" (deref name)
                  (case relationshipRule r of
                     Nullify -> [ inverseRemove config inv ]
                     Cascade -> [ callS ("inverse" /->/ "deleteObject") [] ])
               , callS (name /->/ "removeAll") []
               ])
   cascadeDeleteDoc r =
         vsep $ case relationshipRule r of
                         Nullify -> nullifyRelation r
                         Cascade -> deleteRelation r

   nullifyRelation r =
      [callS (cppSetterNameP AllItems r) [braceList []]]

   deleteRelation r =
      [ autoInit (variableName r) (call ((cppGetterNameP AllItems) r) [])
      , guardP (Con.nesting config) (variableName r)
           [ callS ((if fieldOptional r then parens . deref else id) (variableName r) /->/ "deleteObject") []
           , if fieldOptional r then callS (cppSetterNameP AllItems r) [braceList []] else mempty
           ]
      ] ++ nullifyRelation r

   variableName r             = pretty (relationshipName r) <> "Instance"
   attrs                      = orderedNonTransientAttributes e
   ptype Nothing              = "PersistentObject"
   ptype (Just a)             = pretty a
   templateClass              = ("template class" <+>)
   prologue = doubleLined
      [
         generateInclude PublicInclude config e
      , vsep (map (generateInclude PublicInclude config) (relatedEntitiesForEntity e (Con.configurationEntities config)))
      %% usingNamespace nxAS
      , usingNamespace (nxArb config ("V" <> pretty (Con.configurationVersion config)))
      , using "LocalType" (fieldType (joinNamespaces $ Con.configurationNamespaces config) Internal NoNamespace e)

      , statement (templateClass (sharedPtrOf (fieldType (joinNamespaces $ Con.configurationNamespaces config) Internal NoNamespace e)))
      , statement (templateClass (arrayOf Namespaced (sharedPtrOf (fieldType (joinNamespaces $ Con.configurationNamespaces config) Internal NoNamespace e))))

      %% methodDef (Con.nesting config) (scope [fieldType (joinNamespaces $ Con.configurationNamespaces config) Internal NoNamespace e, "bind"]) [] ["SourceBinder&"<+>"parentBinder"] "void"
            [ callS (ptypeInternal (entityParentName e) /::/"bind") ["parentBinder"]
            , callS ("parentBinder" /./ "bind") (
                  fieldType (joinNamespaces $ Con.configurationNamespaces config) Public RbScoped e :
                  (persistType /::/ ptype (entityParentName e)) :
                  (map (defRelField config e) relns
                  ++ map (\a -> call "ATTRIBUTE" [scope [fieldType (joinNamespaces $ Con.configurationNamespaces config) Internal NoNamespace e, instanceName a], baseName AllItems a, if attributeIndexed a then "Indexed" else "Unindexed"]) attrs))
            ]
      %% methodDef (Con.nesting config)
            (scope [fieldType (joinNamespaces $ Con.configurationNamespaces config) Internal NoNamespace e, "faultObject"])
            []
            []
            void
            ([ guardP (Con.nesting config) "isFaulted()" [statement "return"]
             , callS (ptypeInternal (entityParentName e) /::/ "faultObject") []
             ] ++ map faultRelnDoc relns)
      %% methodDef (Con.nesting config)
            (scope [fieldType (joinNamespaces $ Con.configurationNamespaces config) Internal NoNamespace e, "deleteObject"])
            []
            []
            void
            ([ guardP (Con.nesting config) "isDeleted()" [statement "return"]
             , callS (ptypeInternal (entityParentName e) /::/ "deleteObject") []
             ] ++ map cascadeDeleteDoc relns)
      ]

attributeDef :: Con.Configuration -> Entity -> Attribute -> Doc ann
attributeDef config e a@Attribute{..} = vcat (basics ++ special)
   where
      dNameMaker f = scope [fieldType (joinNamespaces $ Con.configurationNamespaces config) Internal NoNamespace e, f a]
      basics =
          [ pragmaMark (pretty attributeName)
          , methodDef (Con.nesting config) (dNameMaker (cppGetterNameP AllItems)) [ConstF]
               []
               (fieldType (joinNamespaces $ Con.configurationNamespaces config) Public NoNamespace a)
               [ autoInit "contextLock" (call (call "getContext" [] /->/ "ensureUnfaultedAndBorrowLock") ["objectId"])
               , returnInternal (instanceName a)
               ]
          , methodDef (Con.nesting config) (dNameMaker (cppSetterNameP AllItems)) []
               [fieldType (joinNamespaces $ Con.configurationNamespaces config) Public NoNamespace a <+> baseName AllItems a]
               void
               [ autoInit "contextLock" (call (call "getContext" [] /->/ "ensureUnfaultedAndBorrowLock") ["objectId"])
               , callS (call "getContext" [] /->/ "updateObject") ["objectId", instanceName a, baseName AllItems a]
               ]
          , line
          ]
      special =
         case attributeType of
            AttributeTransformable _ -> [
               methodDef (Con.nesting config) (dNameMaker (cppSetterNameP AllItems)) []
                  [tupleOf ["size_t", constPtrTo (nxaNS NoNamespace "byte")] <+> baseName AllItems a]
                  void
                  [callS (dNameMaker (cppSetterNameP AllItems))
                     [ call ("Blob"/::/"withMemoryAndSize")
                        [ stdGet "1" (baseName AllItems a)
                        , stdGet "0" (baseName AllItems a)
                        ]]]
               ]
            _ -> []


openTag :: Doc ann -> Doc ann
openTag a = "<" <> a <> ">"

closeTag :: Doc ann -> Doc ann
closeTag a = "</" <> a <> ">"

describeRelationship :: Relationship -> [Doc ann]
describeRelationship r =
   [ callS ("output" /./ "append") [call ("childIndentation"/./"indentedLine") [literalString (openTag (baseName AllItems r))]]
   , innerDescribeRelationship r
   , callS ("output" /./ "append") [call ("childIndentation"/./"indentedLine") [literalString (closeTag (baseName AllItems r))]]
   ]

innerDescribeRelationship ::  Relationship -> Doc ann
innerDescribeRelationship r =
   callS ("output" /./ "append") [call (nxA "describe") [instanceName r, "childIndentation"]]

defRelField :: Con.Configuration -> Entity -> Relationship -> Doc ann
defRelField config e r =
   let inv = fromJust (inverseRelationship r (Con.configurationEntities config))
   in call "RELATION"
      [ scope [fieldType (joinNamespaces $ Con.configurationNamespaces config) Internal NoNamespace e, instanceName r]
      , baseName AllItems r
      , pretty (relationshipDest r)
      , if relationshipOrdered r then "Ordered" else "Unordered"
      , if relationshipToMany r then "ToMany" else "ToOne"
      , pretty $ show (relationshipRule r)
      , baseName AllItems inv
 --     , pretty (relationshipDest inv)
      , if relationshipOrdered inv then "Ordered" else "Unordered"
      , if relationshipToMany inv then "ToMany" else "ToOne"
      ]

data LockMode = UnlockOnly | UnlockAndUnfault | UnlockUnfaultAndMarkUpdated

lockedName :: Doc ann
lockedName = "lockedContext"

lockedContext :: LockMode -> (Doc ann -> [Doc ann]) -> [Doc ann]
lockedContext UnlockOnly func =
   autoInit lockedName (call ("context" /./ "lock") []) : autoInit "contextLock" (call (lockedName /->/ "borrowLock") []) : func lockedName
lockedContext UnlockAndUnfault func =
   autoInit lockedName (call ("context" /./ "lock") []) : autoInit "contextLock" (call (lockedName /->/ "ensureUnfaultedAndBorrowLock") ["objectId"]) : func lockedName
lockedContext UnlockUnfaultAndMarkUpdated func =
   autoInit lockedName (call ("context" /./ "lock") []) : autoInit "contextLock" (call (lockedName /->/ "ensureUnfaultedAndBorrowLock") ["objectId"]) : callS (lockedName /->/ "updateObject") ["objectId"] : func lockedName

relationshipDef :: Con.Configuration -> Entity -> Relationship -> Doc ann
relationshipDef config e r | relationshipToMany r =
      vsep [ pragmaMark ("to-many relationship:" <+> baseName AllItems r)
           , methodDef (Con.nesting config) (dGetterName config AllObjectIDs e r) [ConstF] []
                  (optionalAOR NoNamespace r (arrayOf NoNamespace "ObjectID"))
                  (lockedContext UnlockAndUnfault (\_locked ->
                     if fieldOptional r
                     then
                        [ guardP (Con.nesting config) (negateP $ instanceName r) [ returnS "nothing"]
                        , returnS (parens (deref (instanceName r)) /->/ call "objectIDs" [])
                        ]
                     else
                        [ returnS (instanceName r /->/ call "objectIDs" [])
                        ]
                  )
                  )
           , methodDef (Con.nesting config) (dGetterName config AllObjectIDsOfType e r) [ConstF] [typename (schemaType /::/ "Type") <+> "type"]
                       (optionalAOR NoNamespace r (setOf NoNamespace "ObjectID"))
                  (lockedContext UnlockOnly (\locked ->
                    (if fieldOptional r
                     then
                        [ guardP (Con.nesting config) (instanceName r /&&/ call (parens (deref (instanceName r)) /->/ "isRelationshipFaulted") [])
                           [ statement $ template "SomeSourceBinder" [schemaType] <+> "binder" <> braceList [deref (locked /->/ "source"), "Mode::Load", "objectId"]
                           , returnS $ call ("source"/./"loadAllOfTypeFromToMany") ["type", defRelField config e r]
                           ]
                        ]
                     else
                        [ guardP (Con.nesting config) (call (instanceName r /->/ "isRelationshipFaulted") [])
                           [ statement $ template "SomeSourceBinder" [schemaType] <+> "binder" <> braceList [deref (locked /->/ "source"), "Mode::Load", "objectId"]
                           , returnS $ call ("binder"/./"loadAllOfTypeFromToMany") ["type", defRelField config e r]
                           ]
                        ]
                     ) ++
                      [ statement (mutableSetOf NoNamespace "ObjectID" <+> "result")
                      , autoForP (Con.nesting config) "oid" (call (instanceName r /->/ "objectIDs") [])
                        [ guardP (Con.nesting config) (call ("RBSchema"/::/"typeIs") ["oid", "type"]) [callS ("result"/./"add") ["oid"]]
                        ]
                      , returnS "result"
                      ]
                  ))
           , methodDef (Con.nesting config) (dLoaderName config AllItems e r) [ConstF] ["bool" <+> "load"]
                  "count"
                  (lockedContext UnlockAndUnfault (\locked ->
                     if fieldOptional r
                     then
                        [ guardP (Con.nesting config) (instanceName r /&&/ call (parens (deref (instanceName r)) /->/ "isRelationshipFaulted") [])
                           [ statement $ template "SomeSourceBinder" [schemaType] <+> "binder" <> braceList [deref (locked /->/ "source"), "Mode::Load", "objectId"]
                           , returnS $ call ("source"/./"loadToMany") ["load", defRelField config e r]
                           ]
                        , guardP (Con.nesting config) (negateP (parens (deref (instanceName r))) /->/ "internalVector") [returnS $ pretty (0 :: Int)]
                        , returnS $ parens (deref (instanceName r)) /->/ "internalVector" /->/ call "size" []
                        ]
                     else
                        [ guardP (Con.nesting config) (call (instanceName r /->/ "isRelationshipFaulted") [])
                           [ statement $ template "SomeSourceBinder" [schemaType] <+> "binder" <> braceList [deref (locked /->/ "source"), "Mode::Load", "objectId"]
                           , returnS $ call ("binder"/./"loadToMany") ["load", defRelField config e r]
                           ]
                        , guardP (Con.nesting config) (negateP (instanceName r /->/ "internalVector")) [returnS $ pretty (0 :: Int)]
                        , returnS $ instanceName r /->/ "internalVector" /->/ call "size" []
                        ]
                     )
                  )
           , methodDef (Con.nesting config) (dGetterName config ObjectIDByIndex e r) [ConstF] ["count" <+> "index"]
                  (typename (schemaType /::/ "ObjectID"))
                  (lockedContext UnlockAndUnfault (\_locked ->
                     [ optionalCond config r instanceName
                        (\name ->
                          [ returnS (name /->/ call "getObjectIDAtIndex" ["index"]) ]
                        )
                     ]
                  ))
           , methodDef (Con.nesting config) (dGetterName config AllItems e r <> "Const") [ConstF] []
                  ("const" <+> fieldType (joinNamespaces $ Con.configurationNamespaces config) Public NoNamespace r)
                  (lockedContext UnlockAndUnfault (\_locked ->
                        if fieldOptional r
                        then
                           [ guardP (Con.nesting config) (negateP $ instanceName r) [ returnS "nothing"]
                           , returnS (braceList [parens (deref (instanceName r))])
                           ]
                        else
                           [ returnS (deref (instanceName r))
                           ]
                  ))
           , methodDef (Con.nesting config) (dGetterName config AllItems e r) [] []
                  (fieldType (joinNamespaces $ Con.configurationNamespaces config) Public NoNamespace r)
                  (lockedContext UnlockAndUnfault (\_locked ->
                        if fieldOptional r
                        then
                           [ guardP (Con.nesting config) (negateP $ instanceName r) [ returnS "nothing"]
                           , returnS (braceList [parens (deref (instanceName r))])
                           ]
                        else
                           [ returnS (deref (instanceName r))
                           ]
                  ))
           , methodDef (Con.nesting config) (dOrderName config ItemsByValue e r) [] [
               constReference (arrayOf NoNamespace (sharedPtrOf (internalType (relationshipDest r)))) <+> "objects", "count" <+> "to"]
                  void
                  (lockedContext UnlockUnfaultAndMarkUpdated (\_locked ->
                        if fieldOptional r
                        then
                           [ nxaAssertTrue (instanceName r)
                           , callS (parens (deref (instanceName r)) /->/ "rearrange") [call "std::move" ["objects"], "to"]
                           ]
                        else
                           [ callS (instanceName r /->/ "rearrange") [call "std::move"["objects"], "to"]
                           ]
                  ))
           , methodDef (Con.nesting config) (dOrderName config ItemsByObjectID e r) [] [
               constReference (arrayOf NoNamespace (typename (schemaType /::/ "ObjectID"))) <+> "objectids", "count" <+> "to"]
                  void
                  (lockedContext UnlockUnfaultAndMarkUpdated (\_locked ->
                        if fieldOptional r
                        then
                           [ nxaAssertTrue (instanceName r)
                           , callS (parens (deref (instanceName r)) /->/ "rearrange") [call "std::move" ["objectids"], "to"]
                           ]
                        else
                           [ callS (instanceName r /->/ "rearrange") [call "std::move" ["objectids"], "to"]
                           ]
                  ))
           , methodDef (Con.nesting config) (dOrderName config ItemsByIndex e r) [] [
               constReference (setOf NoNamespace ("count")) <+> "indicies", "count" <+> "to"]
                  void
                  (lockedContext UnlockUnfaultAndMarkUpdated (\_locked ->
                        if fieldOptional r
                        then
                           [ nxaAssertTrue (instanceName r)
                           , callS (parens (deref (instanceName r)) /->/ "rearrange") [call "std::move" ["indicies"], "to"]
                           ]
                        else
                           [ callS (instanceName r /->/ "rearrange") [call "std::move" ["indicies"], "to"]
                           ]
                  ))
           , methodDef (Con.nesting config) (dAdderName config ItemByValue e r) [] [constReference (sharedPtrOf (internalType (relationshipDest r))) <+> "inverse"]
                  void (
                  let inv = fromJust (inverseRelationship r (Con.configurationEntities config))
                  in lockedContext UnlockUnfaultAndMarkUpdated (\locked ->
                     [ callS (locked /->/ "updateObject") ["inverse" /->/ call "objectID" []]
                     , inverseInit config locked "inverse" r inv
                     , inverseAssign locked inv
                     , callS ((if relationshipOptional r then parens.deref else id)
                                 (instanceName r) /->/ "append") ["inverse"]
                     ]))
           , methodDef (Con.nesting config) (dRemoveAllName config e r) [] []
                  void (
                  let inv = fromJust (inverseRelationship r (Con.configurationEntities config))
                  in lockedContext UnlockUnfaultAndMarkUpdated (\locked ->
                     [ optionalCond config r instanceName
                        (\name ->
                            [ autoForP (Con.nesting config) "inverse" (deref name)
                                [ callS (locked /->/ "updateObject") ["inverse"/->/"objectId"]
                                , inverseRemove config inv
                                ]
                            , callS (name /->/ "removeAll") []
                            ])
                     ]))
           , methodDef (Con.nesting config) (dRemoverName config ItemByIndex e r) [] ["count" <+> "index"]
                  void (
                  let inv = fromJust (inverseRelationship r (Con.configurationEntities config))
                  in lockedContext UnlockUnfaultAndMarkUpdated (\locked ->
                     [ optionalCond config r instanceName
                        (\name ->
                            [ autoInit "inverse" (parens (deref name) <> bracketList ["index"])
                            , callS (locked /->/ "updateObject") ["inverse"/->/"objectId"]
                            , callS (name /->/ "removeObjectWithID") ["inverse" /->/ "objectId"]])
                     , inverseRemove config inv
                     ]))
           , methodDef (Con.nesting config) (dRemoverName config ItemByValue e r) [] [constReference (sharedPtrOf (internalType (relationshipDest r))) <+> "inverse"]
                  void (
                  let inv = fromJust (inverseRelationship r (Con.configurationEntities config))
                  in lockedContext UnlockUnfaultAndMarkUpdated (\locked ->
                     [ callS (locked /->/ "updateObject") ["inverse"/->/"objectId"]
                     , optionalCond config r instanceName
                        (\name -> [callS (name /->/ "removeObjectWithID") ["inverse" /->/ "objectId"]])
                     , inverseRemove config inv
                     ]))
           , line
           ]
   where
      inverseAssign locked rr | relationshipToMany rr =
         callS ("inverse" /->/ instanceName rr /->/ "append")
            [ deref $ call (locked /->/ template "fetchObject" [fieldType (joinNamespaces $ Con.configurationNamespaces config) Internal NoNamespace e]) ["objectId"]
            ]
      inverseAssign _locked rr =
         assign ("inverse" /->/ instanceName rr) "objectId"

relationshipDef config e r = vsep
      [ pragmaMark ("to-one relationship:" <+> baseName AllItems r)
      , oidGetterDef config e r
      , getterDef config AllItems e r optionalGetterDef requiredGetterDef
      , setterDef config AllItems e r optionalSetterDef requiredSetterDef
      , line
      ]
   where
      inv = fromJust (inverseRelationship r (Con.configurationEntities config))
      requiredGetterDef locked =
         [ guardP (Con.nesting config) (negateP (instanceName r /./ call "isValid" [])) [ returnS (braceList []) ] -- Some tests don't init required fields, then check things work. delete callbacks fire and will hit this
         , autoInit "fetchedObject" (call (locked /->/ template "fetchObject" [ internalType (relationshipDest r) ]) [instanceName r])
         , guardP (Con.nesting config) (negateP "fetchedObject") [returnS (braceList [])] -- we have to work during cascade-deletes, where the object is already deleted. Can't assert
         , returnS (deref "fetchedObject")
         ]
      optionalGetterDef locked =
         [ guardP (Con.nesting config) (negateP (instanceName r)) [returnDefault]
         , returnS $ braceList [call (locked /->/ template "fetchObject" [ internalType (relationshipDest r) ])
                                     [deref (instanceName r)]]
         ]
      requiredSetterDef _locked =
         [ ifP (Con.nesting config) (negateP "inverse")
            [ assign (instanceName r) (braceList []) ]
            (Just
               [ assign (instanceName r) ("inverse" /->/ "objectId")
               , if relationshipToMany inv
                 then callS ("inverse" /->/ instanceName inv /->/ "append") [call (template "sharedFromThis" [internalType $ relationshipDest inv]) []]
                 else assign ("inverse" /->/ instanceName inv) "objectId"
               ]
            )
         ]
      optionalSetterDef _locked =
         [ ifP (Con.nesting config) (negateP "inverse")
            [ assign (instanceName r) "nothing" ]
            (Just
               [ autoInit "derel" (deref "inverse")
               , assign (instanceName r) ("derel" /->/ "objectId")
               , if relationshipToMany inv
                 then callS ("derel" /->/ instanceName inv /->/ "append") [call (template "sharedFromThis" [internalType $ relationshipDest inv]) []]
                 else assign ("derel" /->/ instanceName inv) "objectId"
               ]
            )
         ]

inverseInit :: Con.Configuration -> Doc ann -> Doc ann -> Relationship -> Relationship -> Doc ann
inverseInit _config _locked _d r rr | relationshipToMany rr && relationshipToMany r = mempty
inverseInit config locked d r rr | relationshipToMany r =
   optionalCond' config rr (\a -> d /->/ instanceName a) id (\name ->
      [ autoInit "fetchedObject"
         (call (locked /->/ template "fetchObject" [ internalType (relationshipDest rr) ]) [name])
      , guardP (Con.nesting config) "fetchedObject"
         [ callS ((parens.deref $ "fetchedObject") /->/ instanceName r /->/ "removeObjectWithID")
            ["objectId"]
         ]
      ])
inverseInit config locked d r rr =
   optionalCond' config rr ((d /->/) . instanceName) id (\name ->
      [ autoInit "fetchedObject"
         (call (locked /->/ template "fetchObject" [ internalType (relationshipDest rr)]) [name])
      , guardP (Con.nesting config) "fetchedObject"
         [ assign ((parens.deref $ "fetchedObject") /->/ instanceName r) (braceList []) ]
      ])

inverseRemove :: Con.Configuration -> Relationship -> Doc ann
inverseRemove config rr | relationshipToMany rr =
   optionalCond config rr (\a -> "inverse" /->/ instanceName a)
      (\name -> [callS (name /->/ "removeObjectWithID") ["objectId"]])
inverseRemove _config rr =
   assign ("inverse" /->/ instanceName rr) (braceList [])

internalName :: AOR a => Con.Configuration -> (a -> Doc ann) -> Entity -> a -> Doc ann
internalName config name e r = fieldType (joinNamespaces $ Con.configurationNamespaces config) Internal NoNamespace e /::/ name r

dCacheName :: AOR a => a -> Doc ann
dCacheName r             = fieldName r <> "Cache"

dGetterName, dAdderName, dLoaderName, dOrderName, dRemoverName, dSetterName :: AOR a => Con.Configuration -> ValueOrIndex -> Entity -> a -> Doc ann
dGetterName config c     = internalName config (cppGetterNameP c)
dAdderName config c      = internalName config (cppAdderNameP c)
dLoaderName config c     = internalName config (cppLoaderNameP c)
dOrderName config c      = internalName config (cppOrderNameP c)
dRemoverName config c    = internalName config (cppRemoverNameP c)
dSetterName config c     = internalName config (cppSetterNameP c)

dRemoveAllName :: AOR a => Con.Configuration -> Entity -> a -> Doc ann
dRemoveAllName config    = internalName config cppRemoveAllNameP

valueForDescription :: Attribute -> Doc ann
valueForDescription a =
   convertToCocoaFormatStrValue (call (cppGetterNameP AllItems a) []) (fieldOptional a) (attributeType a)

getterDef, setterDef :: Con.Configuration -> ValueOrIndex -> Entity -> Relationship -> (Doc ann -> [Doc ann]) -> (Doc ann -> [Doc ann]) -> Doc ann
getterDef config c e r og rg =
    methodDef (Con.nesting config) (dGetterName config c e r) [ConstF] [] (optionalAOR NoNamespace r (sharedPtrOf (internalType (relationshipDest r)))) (
      lockedContext UnlockAndUnfault (\locked -> if fieldOptional r then og locked else rg locked))
setterDef config c e r og rg =
    methodDef (Con.nesting config) (dSetterName config c e r) [] [optionalAOR NoNamespace r (sharedPtrOf (internalType (relationshipDest r))) <+> "inverse"] void (
      lockedContext UnlockUnfaultAndMarkUpdated (\locked ->
         inverseInit config locked "this" (fromJust (inverseRelationship r (Con.configurationEntities config))) r : if fieldOptional r then og locked else rg locked))

oidGetterDef :: Con.Configuration -> Entity -> Relationship -> Doc ann
oidGetterDef config e r =
   methodDef (Con.nesting config) (dGetterName config AllItems e r <> "ObjectID") [ConstF] []
      (optionalAOR NoNamespace r oid)
      [returnS (instanceName r)]
 where oid = schemaType /::/ "ObjectID"

formatStrForAttribute :: Attribute -> Doc ann
formatStrForAttribute a@Attribute { attributeType = AttributeInteger _ } = cppGetterNameP AllItems a <> "=\"%d\""
formatStrForAttribute a = cppGetterNameP AllItems a <> "=\"%s\""


convertToCocoaFormatStrValue :: Doc ann -> Bool -> AttributeType -> Doc ann
convertToCocoaFormatStrValue doc _ _a = call "describe" [doc, "state"]


