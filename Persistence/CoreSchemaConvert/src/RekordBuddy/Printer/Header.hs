{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module RekordBuddy.Printer.Header where

import           Data.Text.Prettyprint.Doc
import qualified Data.Map.Lazy as Map
import           RekordBuddy.Types
import Data.Text(Text)
import           RekordBuddy.Helpers
import           RekordBuddy.Printer.NxA
import qualified RekordBuddy.Configuration as Con
import Data.Maybe (maybe)
import Data.List (sort)

headerDoc :: Con.Configuration -> Entity -> Doc ann
headerDoc config e =
   vsep
      [ copyright
      , includeRoot config ((pretty $ head (Con.configurationNamespaces config)) <> "Schema.hpp")
      , includeSystem "Persistence/GenericPersistentObject.hpp"
      , maybe mempty (generateInclude PublicInclude config) (entityForName (entityParentName e) (Con.configurationEntities config))
      , pragmaOnce
      , embedInNamespace ([nxAT] ++ Con.configurationNamespaces config) (
         namespace 0 (Con.defaultVersion config) ("V" <> pretty (Con.configurationVersion config)) [
               classDecl (Con.nesting config) Class internalName
                                       [public <+> ptypeInternal (entityParentName e)]
                                       [ using "LocalType" internalName
                                       , classDeclBody attrs relns]
               ]
        )
      ]
 where attrs                          = orderedNonTransientAttributes e
       rawrelns                       = sort $ Map.elems $ entityRelationships e
       relns                          = toOneRelations ++ toManyRelations
       ptypeInternal Nothing          = template "GenericPersistentObject" ["RBSchema"]
       ptypeInternal (Just a)         = internalType a
       toOneRelations                 = filter (not . relationshipToMany) rawrelns
       toManyRelations                = filter relationshipToMany rawrelns
       internalName                   = fieldType (joinNamespaces $ Con.configurationNamespaces config) Internal NoNamespace e
       relationshipToOneInit _ r      = [instanceName r <> braces mempty]
       relationshipToManyInit _ r     =
         [ instanceName r <> braces (makeUnique [call "decltype" [instanceName r] /::/ "element_type"]
            [ "this", "&" <> (internalName /::/ cppLoaderNameP AllItems r)])
         ]
       fieldInits emptyObj =
           map (append (braceList []) . instanceName) attrs ++
           concatMap (relationshipToOneInit emptyObj) toOneRelations ++
           concatMap (relationshipToManyInit emptyObj) toManyRelations
       serialization =
           [ pragmaMark "Serialization"
           , methodDeclStmt "bind" [OverrideF] ["SourceBinder" <+> "&parentBinder"] "void"
           , line
           ]
       ctorsDtors =
         [ pragmaMark "Constructors, Destructors, Friends, Move and Copy"
         , protectedAccess (Con.nesting config)
         , statement ("friend" <+> "struct" <+> schemaType)
         , vsep [internalName <> parenCommaList [ persistType <+> "type", "Context" <+> "context"]
                , indent (Con.nesting config)
                    (":" <+> initialize internalName [initialize "ObjectID" ["type"], call ("std"/::/"move") ["context"]])
                , braceList [nxaAssertTrue(call (schemaType/::/"typeIs") ["type", template "TypeEnumeration" [internalName] /::/ "value"])] 
                ]
         , publicAccess (Con.nesting config)
         , vsep [(internalName <> parenCommaList ["ObjectID"<+>"id", "Context"<+>"context"])
                , indent (Con.nesting config)
                    (":" <+> (commaList ((ptypeInternal (entityParentName e) <> braceList ["id", "context"]) : fieldInits False)))
                , braceList [nxaAssertTrue ( call (schemaType/::/"typeIs") [call ("id" /./ "objectType") [], template "TypeEnumeration" [internalName] /::/ "value"] ) ]
                ]
         ,  (if Con.configurationCocoaWrappers config
             then hangInMethod (Con.nesting config) ("~" <> internalName <> parens mempty)
                       [
                        guardP (Con.nesting config) "cocoaWrapper"
                           [ callS (schemaType /::/ "releaseWrapperFor") [call (template "sharedFromThis" [internalName]) []]
                           , assign "cocoaWrapper" "nullptr"
                           ]
                       ]
             else statement ("~" <> internalName <> parens mempty <+> "override" /=/ "default"))
            , defaultCopyCtorDef EqualsDelete internalName
            , defaultMoveCtorDef EqualsDefault internalName
            , defaultCopyAssignDef EqualsDelete internalName
         , line
         ]
       queryAttribute a@Attribute{..} | attributeType == AttributeString =
         templateScope [typename "T" /=/ internalName] $
         methodDef (Con.nesting config) (cppFetchByNameP a) [StaticF]
                   ["Context" <+> "context", fieldType (joinNamespaces $ Con.configurationNamespaces config) Public NoNamespace a <+> "query", "QueryOperation" <+> "op"/=/"QueryOperation" /::/ "Equals"]
                   (arrayOf NoNamespace (sharedPtrOf "T"))
            [ callS "static_assert" [ call (schemaType/::/"typeIs") [ template "TypeEnumeration" ["T"] /::/ "value"
                                                                    , template "TypeEnumeration" [internalName] /::/ "value"]
                                    , literalString ("Must only query" <+> internalName <+> "or subtypes")]
            , line <> pragmaMark (if attributeIndexed then "indexed" else "unindexed")
            , returnS (call ("context"/->/ "template" <+> template "fetchObjectsByAttribute" [internalName, "T"])
                  [ literalString (baseName AllItems a)
                  , template "TypeEnumeration" ["T"] /::/ "value"
                  , addressOf (internalName /::/ instanceName a)
                  , "nothing"
                  , (if attributeOptional then braces else id) "query"
                  , "op"
                  ]
               )
            ]
       queryAttribute a@Attribute{..} =
         templateScope [typename "T" /=/ internalName] $
         methodDef (Con.nesting config) (cppFetchByNameP a) [StaticF]
                   ["Context" <+> "context", fieldType (joinNamespaces $ Con.configurationNamespaces config) Public NoNamespace a <+> "query"]
                   (arrayOf NoNamespace (sharedPtrOf "T"))
            [ callS "static_assert" [ call (schemaType/::/"typeIs") [ template "TypeEnumeration" ["T"] /::/ "value"
                                                                    , template "TypeEnumeration" [internalName] /::/ "value"]
                                    , literalString ("Must only query" <+> internalName <+> "or subtypes")]
            , pragmaMark (if attributeIndexed then "indexed" else "unindexed")
            , returnS (call (template ("context"/->/"template" <+> "fetchObjectsByAttribute") [internalName, "T"])
                  [ literalString (baseName AllItems a)
                  , template "TypeEnumeration" ["T"] /::/ "value"
                  , addressOf (internalName /::/ instanceName a)
                  , "nothing"
                  , (if attributeOptional then braces else id) "query"
                  ]
               )
            ]
       queries = map queryAttribute attrs
       deleter =
         [ methodDeclStmt "deleteObject" [OverrideF] [] void
         , methodDeclStmt "faultObject" [OverrideF] [] void
         ]
       attributesAndRelationships = [
            pragmaMark "Attributes & Relationships"
          ] ++ map (attributeDecl config e) attrs
            ++ map (relationshipDecl config e) relns
            ++ [line]
       classDeclBody as rs =
           vsep
            (
            map (instanceDeclaration config) as ++
            map (relationshipInstanceDecl config) rs ++
            ctorsDtors ++
            queries ++
            deleter ++
            attributesAndRelationships ++ serialization
            )

attributeDecl :: Con.Configuration -> Entity -> Attribute -> Doc ann
attributeDecl config _e a@Attribute{..} = vcat (basics ++ special) <> line
   where
      basics =
          [ pragmaMark (dtext attributeName)
          , methodDeclStmt (cppGetterNameP AllItems a) [ConstF]
               []
               (fieldType (joinNamespaces $ Con.configurationNamespaces config) Public NoNamespace a)
          , methodDeclStmt (cppSetterNameP AllItems a) []
               [fieldType (joinNamespaces $ Con.configurationNamespaces config) Public NoNamespace a <+> baseName AllItems a]
               void
          ]
      special =
         case attributeType of
            AttributeTransformable _ -> [
               methodDeclStmt (cppSetterNameP AllItems a) []
                  [tupleOf ["size_t", constPtrTo (nxaNS NoNamespace "byte")] <+> baseName AllItems a]
                  void
               ]
            _ -> []


methodDeclStmt :: Doc ann -> [Flag] -> [Doc ann] -> Doc ann -> Doc ann
methodDeclStmt n f a r = statement (methodDecl n f a r)

relationshipDecl :: Con.Configuration -> Entity -> Relationship -> Doc ann
relationshipDecl config _e  r | relationshipToMany r = vsep
    [ pragmaMark (fieldName r)
    , methodDeclStmt (cppGetterNameP AllObjectIDs r) [ConstF] []
            (optionalAOR NoNamespace r (arrayOf NoNamespace (typename (schemaType /::/ "ObjectID"))))
    , methodDeclStmt (cppGetterNameP AllObjectIDsOfType r) [ConstF] [typename (schemaType /::/ "Type")]
            (optionalAOR NoNamespace r (setOf NoNamespace (typename (schemaType /::/ "ObjectID"))))
    , methodDeclStmt (cppGetterNameP AllItems r) [] []  (fieldType (joinNamespaces $ Con.configurationNamespaces config) Public NoNamespace r)
    , methodDeclStmt (cppGetterNameP AllItems r <> "Const") [ConstF] []  ("const"<+> fieldType (joinNamespaces $ Con.configurationNamespaces config) Public NoNamespace r)
    , methodDeclStmt (cppLoaderNameP AllItems r) [ConstF] ["bool"] "count"
    , methodDeclStmt (cppGetterNameP ObjectIDByIndex r) [ConstF] ["count"] (typename (schemaType /::/ "ObjectID"))
    , methodDeclStmt (cppAdderNameP ItemByValue r) [] [constReference . sharedPtrOf $ internalType (relationshipDest r)] void
    , methodDeclStmt (cppRemoverNameP ItemByValue r) [] [constReference . sharedPtrOf $ internalType (relationshipDest r)] void
    , methodDeclStmt (cppRemoverNameP ItemByIndex r) [] ["count"] void
    , methodDeclStmt (cppRemoveAllNameP r) [] [] void
    , methodDeclStmt (cppOrderNameP ItemsByValue r) [] [constReference $ arrayOf NoNamespace (sharedPtrOf $ internalType (relationshipDest r)), "count"] void
    , methodDeclStmt (cppOrderNameP ItemsByObjectID r) [] [constReference $ arrayOf NoNamespace (typename (schemaType /::/ "ObjectID")), "count"] void
    , methodDeclStmt (cppOrderNameP ItemsByIndex r) [] [constReference $ setOf NoNamespace ("count"), "count"] void
    , line
    ]
relationshipDecl _ _e r =
      vsep ((pragmaMark (fieldName r)) : (basics ++ special))
   where
      basics =
         [ methodDeclStmt ((cppGetterNameP AllItems r)<>"ObjectID") [ConstF] [] (optionalAOR NoNamespace r (schemaType /::/ "ObjectID"))
         , methodDeclStmt (cppGetterNameP AllItems r) [ConstF] [] (optionalAOR NoNamespace r (sharedPtrOf (internalType (relationshipDest r))))
         , methodDeclStmt (cppSetterNameP AllItems r) [] [optionalAOR NoNamespace r (sharedPtrOf (internalType (relationshipDest r)))] void
         ]
      special = []


instanceDeclaration :: AOR a => Con.Configuration -> a -> Doc ann
instanceDeclaration config a = statement (fieldType (joinNamespaces $ Con.configurationNamespaces config) Public NoNamespace a <+> instanceName a)

relationshipInstanceDecl :: Con.Configuration -> Relationship -> Doc ann
relationshipInstanceDecl config r =
      vsep [ statement ("friend class" <+> internalType (relationshipDest r))
           , statement (fieldType (joinNamespaces $ Con.configurationNamespaces config) Internal NoNamespace r <+> instanceName r)
           ]

