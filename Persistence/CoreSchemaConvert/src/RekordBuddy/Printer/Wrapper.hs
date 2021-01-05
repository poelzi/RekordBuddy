{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module RekordBuddy.Printer.Wrapper(objcCategoryImplDoc, memberName) where

import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc
import qualified Data.Map.Lazy as Map
import           RekordBuddy.Types
import qualified RekordBuddy.Configuration as Con
import           RekordBuddy.Helpers
import           RekordBuddy.Printer.NxA


objcCategoryImplDoc :: Con.Configuration -> Entity -> Doc ann
objcCategoryImplDoc config e =
    vsep
    [  copyright
    ,  imports
    ,  declareRelations
    ,  vcat [usingNamespace nxAS , usingNamespace (nxArb config ("V" <> pretty (Con.configurationVersion config)))]
    ,  objcImplementation (name <+> parenCommaList ["CppCategory"])
     ([ objcClassMethodDef (Con.nesting config) (nxATypeS config)
         ["objectType"]
         [ returnS (nxAType config (fieldName e)) ]
      ,  objcMemberMethodDef (Con.nesting config) void
         [ setMemberName, parens localPtr <> "object" ]
         [ ifP (Con.nesting config) ("object"/->/"cocoaWrapper")
               [ assign (ptrTo name <+> "transferredWrapper")
                           (parens ("__bridge_transfer" <+> ptrTo name) <> "object" /->/ "cocoaWrapper")
               , nxaAssertNotNil "transferredWrapper" <+> pragmaMark "release any existing wrapper"
               , assign ("object" /->/ "cocoaWrapper") "nullptr"
               ] Nothing
         , assign ("object"/->/"cocoaWrapper") (parens ("__bridge_retained" <+> ptrTo "void")<>"self")
         , assign ("self"/->/"cppBaseObject") (staticCastSharedPtr "PersistentObject" "object")
         , assign ("self"/->/"objectid") ("object" /->/ call "objectID" [])
         ]
      ,  objcMemberMethodDef (Con.nesting config) localPtr
         [memberName]
         [ autoInit "baseobject" (staticCastSharedPtr ename ("self" /->/ "cppBaseObject"))
         , nxaAssertNotNull "baseobject"
         , returnS "baseobject"
         ]
      ] ++ [pragmaMark "Relationships"] ++ map (relationshipDef config (Con.configurationEntities config) e) relns
        ++ [pragmaMark "Attributes"] ++ map (attributeDefClean config e) attrs
        )
      ]
 where attrs         = orderedNonTransientAttributes e
       relns         = Map.elems $ entityRelationships e
       ename         = fieldType (joinNamespaces (Con.configurationNamespaces config)) Internal NoNamespace e
       name          = nxAS <> fieldName e
       localPtr      = sharedPtrOf ename
       declareRelations =
         vcat $ map (\r -> objcImport (nxAS <> dtext (relationshipDest r) <> ".h")) relns
       imports =
         vcat [ objcImportSystem "objc/runtime.h"
              , objcImport (name <> ".h")
              , objcImport (name <> "+CppCategory.h")
              , objcImport "NSData+NxAUtility.h"
              , objcImport "NxAPersistentObjectWrapper.h"
              -- TODO: All relationships of this type should be included too
              ]

memberName, member, setMemberName :: Doc ann
memberName    = "cppObject"
member = "self" /./ memberName
setMemberName = "setCppObject"

attributeDefClean :: Con.Configuration -> Entity -> Attribute -> Doc ann
attributeDefClean config _e a@Attribute{..} = setter %% getter
 where unconvertedTo arg = member /->/ call (cppGetterNameP AllItems a) arg
       (prepFrom, valueFrom) = convertFromCocoa attributeType "value"
       (prepTo, valueTo) = convertToCocoa attributeType (if attributeOptional then parens (deref "cppValue") else "cppValue")
       setter = objcMemberMethodDef (Con.nesting config) void ["set" <> ucaseAtrName a, parens (attributeTypeToCocoaType attributeType) <> "value"]
                       (if attributeOptional
                          then [ ifP (Con.nesting config) "value"
                                    (prepFrom ++ [callS (member /->/ cppSetterNameP AllItems a) [valueFrom]])
                                    (Just [callS (member /->/ cppSetterNameP AllItems a) [optionalNotPresent]])]
                          else prepFrom ++ [callS (member /->/ cppSetterNameP AllItems a) [valueFrom]]
                       )
       getter = objcMemberMethodDef (Con.nesting config) (attributeTypeToCocoaType attributeType) [ dtext attributeName ] (
                        autoInit "cppValue" (unconvertedTo []) : (
                           if attributeOptional
                           then [ ifP (Con.nesting config) "cppValue"
                                       (prepTo ++ [returnS valueTo])
                                       (Just [returnS "nil"])
                                ]
                           else prepTo ++ [returnS valueTo]
                        ))


ucaseAtrName :: Attribute -> Doc ann
ucaseAtrName Attribute{..} = ucaseFirst . T.unpack $ attributeName

relationshipDef :: Con.Configuration -> EntityMap -> Entity -> Relationship -> Doc ann
relationshipDef config _ents _e r | relationshipToMany r =
      getter %% getterFocus %% adder %% remover
   where
      relationTypeDoc = nxAS <> dtext (relationshipDest r)
      unconvertedTo arg = member /->/ call (cppGetterNameP AllItems r) arg
      getterFocus = objcMemberMethodDef (Con.nesting config) (setForRelation False r) [wrapperGetterNameP r <> "Of", parens persistType <> "focus"]
                         (autoInit "cppResults" (unconvertedTo []) :
                           [ assign (setForRelation True r <+> "result") (objcSend (setTypeForRelation True r) [setDefaultForRelation r])
                           , optionalCond config r (const "cppResults") (\name ->
                              [ nxaAssertTrue (call (schemaType /::/ "typeIs") ["focus", persistType /::/ dtext (relationshipDest r)])
                              , autoForP (Con.nesting config) "cppResult" name
                                 [ guardP (Con.nesting config) (call (schemaType /::/ "typeIs") [call ("cppResult"/->/"objectID") [], "focus"])
                                    [ statement $ objcSend "result" ["addObject", call (nxA "wrapperFor") ["cppResult"]] ]
                                 ]
                              ]
                              )
                           , returnS "result"
                           ]
                           )
      getter = objcMemberMethodDef (Con.nesting config) (setForRelation False r) [wrapperGetterNameP r]
                         (autoInit "cppResults" (unconvertedTo []) :
                           [ assign (setForRelation True r <+> "result") (objcSend (setTypeForRelation True r) [setDefaultForRelation r])
                           , optionalCond config r (const "cppResults") (\name ->
                                 [ autoForP (Con.nesting config) "cppResult" name
                                    [ statement $ objcSend "result" ["addObject", call (nxA "wrapperFor") ["cppResult"]]
                                 ]
                                 ])
                           , returnS "result"
                           ]
                           )
      adder = objcMemberMethodDef (Con.nesting config) void [wrapperAdderNameP r, parens (ptrTo relationTypeDoc) <> "value"]
                  [
                  statement $ call ("self"/./"cppObject"/->/cppAdderNameP AllItems r) ["value"/./"cppObject"]
                  ]
      remover = objcMemberMethodDef (Con.nesting config) void [wrapperRemoverNameP r, parens (ptrTo relationTypeDoc) <> "value"]
                  [
                  statement $ call ("self"/./"cppObject"/->/cppRemoverNameP AllItems r) ["value"/./"cppObject"]
                  ]


relationshipDef config _e _ents r@Relationship{..} =
         setter %% getter
   where
      relationTypeDoc = nxAS <> dtext relationshipDest
      unconvertedTo arg = member /->/ call (cppGetterNameP AllItems r) arg
      getter = objcMemberMethodDef (Con.nesting config) (ptrTo relationTypeDoc) [wrapperGetterNameP r]
                        [ returnS (call (nxA "wrapperFor") [unconvertedTo []])
                        ]
      setter = objcMemberMethodDef (Con.nesting config) void [wrapperSetterNameP r, parens (ptrTo relationTypeDoc) <> "value"]
                  [ ifP (Con.nesting config) "value"
                     [ autoInit "cppResult" (objcSend "value" ["cppObject"])
                     , nxaAssertNotNil "cppResult"
                     , statement (member /->/ call (cppSetterNameP AllItems r) ["cppResult"])
                     ]
                     (Just [statement (member /->/ call (cppSetterNameP AllItems r) [if fieldOptional r then optionalNotPresent else braces mempty])])]

