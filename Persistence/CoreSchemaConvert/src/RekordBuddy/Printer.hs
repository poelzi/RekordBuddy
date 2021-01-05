{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module RekordBuddy.Printer( renderEntity
                        , renderRegistrationHeader
                        , renderMetaMacroHeader
                        , renderCMakeLists
                        , renderVersionBody
                        , renderVersionFile
                        , renderAllObjectiveCWrapperHeaders
                        ) where

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.String
import           Data.Function(on)
import           Data.List (sortBy,nub)
import           Data.Maybe (fromMaybe)
import qualified Data.Map.Lazy as Map
import           RekordBuddy.Types
import           RekordBuddy.Helpers
import           RekordBuddy.Printer.NxA
import           RekordBuddy.Printer.Header
import qualified RekordBuddy.Configuration as Con
import           RekordBuddy.Printer.Body
import           RekordBuddy.Printer.CDMigrator
import           RekordBuddy.Printer.Wrapper
import           RekordBuddy.Printer.Test

nxaRender :: Doc ann -> String
nxaRender d = renderString (layoutPretty defaultLayoutOptions {layoutPageWidth = AvailablePerLine 200 0.8} d)

renderEntity :: Con.Configuration -> Entity -> (String, String, String, String, String, String)
renderEntity config ent =
   ( nxaRender $ headerDoc config ent
   , nxaRender $ bodyDoc config ent
   , if Con.configurationCocoaWrappers config then nxaRender $ objcCategoryImplDoc config ent else nxaRender mempty
   , if Con.configurationCocoaWrappers config then nxaRender $ objectiveCWrapperHeader config ent else nxaRender mempty
   , nxaRender $ entityTestDoc config ent
   , nxaRender $ migratorDoc config ent
   )


renderRegistrationHeader :: Con.Configuration -> String
renderRegistrationHeader config = nxaRender (registrationHeaderDoc config)

renderMetaMacroHeader :: Con.Configuration -> String
renderMetaMacroHeader config = nxaRender (metaMacroDoc config)

renderAllObjectiveCWrapperHeaders :: Con.Configuration -> String
renderAllObjectiveCWrapperHeaders config =
      nxaRender renderIncludes
  where
   entities = Map.elems (Con.configurationEntities config)
   sortedEntities = sortBy (compare `on` stringFormInclude (Con.configurationEntities config)) entities
   renderIncludes = vsep
      [ copyright
      , pragmaOnce
      , vcat (map (generateInclude ObjcInclude config) sortedEntities)
      ]


metaMacroDoc :: Con.Configuration -> Doc ann
metaMacroDoc config =
   let entities = Map.elems (Con.configurationEntities config)
   in vsep
      [ copyright
      , "#undef ALL_TYPES"
      , "#define ALL_TYPES(F) " <> vcat (punctuate "\\" (map (\x -> "    F(" <> fieldName x <> "," <> pretty (fromMaybe "PersistentObject" (entityParentName x)) <>") ") entities)) <> mempty <> mempty
      , "#undef ALL_M2M_TYPES"
      , "#define ALL_M2M_TYPES(F) " <> vcat (punctuate "\\" (map (\x -> "    F("
                     <> pretty (relationshipDest (leftRelation x))
                     <> ","
                     <> baseName AllItems (leftRelation x)
                     <> ","
                     <> pretty (relationshipDest (rightRelation x))
                     <> ","
                     <> baseName AllItems (rightRelation x)
                     <>") ") (Con.configurationManyToMany config))) <> mempty <> mempty
      ]

registrationHeaderDoc :: Con.Configuration -> Doc ann
registrationHeaderDoc config =
   let entities = Map.elems (Con.configurationEntities config)
       sortedEnties = sortBy (compare `on` stringFormInclude (Con.configurationEntities config)) entities
   in vsep
      [ copyright
      , pragmaOnce
      , includeSystem "memory"
      , vcat (map (generateInclude PublicInclude config) sortedEnties)
      , vcat
        [ "template <typename F>"
        , methodDef (Con.nesting config) (nxA (joinNamespaces $ Con.configurationNamespaces config)/::/"V" <> pretty (Con.configurationVersion config) /::/ schemaType /::/ "applyToTypes") [] ["F callable"] void
            [
            "ALL_TYPES(CALL_ON_TYPE)"
            ]
        ]
      ]

attributePropDecl :: Con.Configuration -> Entity -> Attribute -> Doc ann
attributePropDecl _config _e Attribute{..} =
   statement ("@property ()" <+> attributeTypeToCocoaType attributeType <+> pretty attributeName)

relationshipPropDecl :: Con.Configuration -> Entity -> Relationship -> Doc ann
relationshipPropDecl config _e r@Relationship{..} | relationshipToMany = vcat
         [ statement ("@property" <+> parens "readonly" <+> setForRelation False r <+> pretty relationshipName)
         , statement (objcMemberMethodDecl void [wrapperAdderNameP r, parens (ptrTo relationTypeDoc) <> "value"])
         , statement (objcMemberMethodDecl (setForRelation False r) [pretty relationshipName <> "Of", parens (nxATypeS config) <> "focus"])
         , statement (objcMemberMethodDecl void [wrapperRemoverNameP r, parens (ptrTo relationTypeDoc) <> "value"])
         ]
   where
      relationTypeDoc = nxAS <> pretty relationshipDest
relationshipPropDecl _config _e Relationship{..} =
   statement ("@property" <+> parens mempty <+> ptrTo(nxAS <> pretty relationshipDest) <+> pretty relationshipName)

renderVersionBody :: Con.Configuration -> String
renderVersionBody config = nxaRender $
   vsep [ copyright
        , includeRoot config ((pretty $ head (Con.configurationNamespaces config)) <>"Schema.hpp")
        , if Con.configurationCocoaWrappers config then
          vcat [ "#ifdef __OBJC__"
               , objcImport (pretty (Con.configurationRoot config) <> "/Persistence/V" <> pretty (Con.configurationVersion config) <> "/AllObjectiveCWrapperTypes.h")
               , "#endif"
               ]
         else mempty
        , includeSchema config "AllPersistentObjects.ipp"
        , includeSchema config "MetaMacroAllTypes.ipp"
        , vcat $ map (\(t, wrap, x) -> wrap (statement $ template ("template" <+> t <+> nxA x) [fullschema]))
               [ ("class", id, "PersistentContext")
               , ("struct", id, "PersistentObjectID")
               , ("struct", id, "GenericPersistentObject")
               , ("struct", id, "SqlLiteBinder")
               , ("struct", id, "SqlLiteStore")
               , ("struct", \x -> vcat ["#if defined(NXA_GENERIC_SOMESOURCE)",x,"#endif"], "ContextBinder")
               , ("struct", \x -> vcat ["#if defined(NXA_GENERIC_SOMESOURCE)",x,"#endif"], "SomeSourceBinder")
               ]
        , statement $ template ("template class" <+> nxA "Variant") [sharedPtrOf (typename (fullschema /::/ "Store")), sharedPtrOf (template (nxA "PersistentContext") [fullschema])]
        , embedInNamespace ([nxAT] ++ Con.configurationNamespaces config) (
          namespace 0 (Con.minVersion config == Con.configurationVersion config) ("V" <> pretty (Con.configurationVersion config))
            [ statement $ "constexpr" <+> template "std::tuple" [schemaType /::/ "SourceVersion", schemaType /::/ "DestinationVersion", schemaType /::/ "MigrateFunctionT" <> "*"] <+> schemaType /::/ "schemaMigrations[]"
            , methodDef (Con.nesting config) (schemaType /::/ "version0ToCurrentMigration") []
               ["const" <+> sharedPtrOf (template "NxA::SqlLiteStore" [schemaType]) <> "&" <+> "store"] void
               [ callS  "applyToTypes"
                  [ lambdaP (Con.nesting config) ["&"] ["auto&& object"]
                     [ statement $ template "SomeSourceBinder" [schemaType] <+> "binder" <> braceList [deref "store", "Mode::Declare", "object.objectID()"]
                     , callS "object.bind" ["binder"]
                     ]
                  ]
               ]
            , methodDef (Con.nesting config) (schemaType/::/"constructDynamic") []
               [ "const ObjectID&" <+> "id"
               , sharedPtrOf (template "PersistentContext" [schemaType]) <+>"c"
               ] (optionalOf NoNamespace (sharedPtrOf (template "GenericPersistentObject" [schemaType])))
               [ switchP (Con.nesting config) "id.objectType()"
                  [ "ALL_TYPES(CONSTRUCT_DYNAMIC)"
                  , "default:"
                  , "return nothing;"
                  ]
               ]
            --, methodDef (Con.nesting config) (schemaType /::/ "ensureCocoaWrapper") []
            --   [ constReference (sharedPtrOf "PersistentObject") <+> "object"
            --   , "RootContext" <+> "context"
            --   ] void
            --   [ guardP (Con.nesting config) "!object" [ statement "return" ]
            --   , guardP (Con.nesting config) "object->cocoaWrapper" [ statement "return" ]
            --   , switchP (Con.nesting config) "object->objectID().objectType()"
            --      [ "ALL_TYPES(ASSIGN_WRAPPER)"
            --      , "default:"
            --      , "return;"
            --      ]
            --   ]
            ])
        ]
 where fullschema = nxA (joinNamespaces $ Con.configurationNamespaces config) /::/ "V" <> pretty (Con.configurationVersion config) /::/ schemaType

renderCMakeLists :: Con.Configuration -> String
renderCMakeLists config =
   nxaRender (vsep
        [ genericCopyright ("# " <>)
        , "cmake_minimum_required(VERSION 3.13)"
        , "project(" <> targetName <> ")"
        , nest 4 ("add_library" <> parens (align (vsep
            ([targetName, "Objects/V" <> pretty (Con.configurationVersion config)  <> "/V" <>pretty (Con.configurationVersion config)  <>"Schema.cpp"] ++ (map makeCppName sortedEntities) ++ [line]))))
        , nest 4 (vsep
            [ "if(APPLE)"
            , "find_library(SecurityFramework Security)"
            , "find_library(IOKitFramework IOKit)"
            , "find_library(CoreFoundationFramework CoreFoundation)"
            , "set(PlatformLibs ${IOKitFramework} ${SecurityFramework} ${CoreFoundationFramework})"
            , "mark_as_advanced(SecurityFramework CoreFoundationFramework)"
            , "target_link_libraries(" <> targetName <> " ${PlatformLibs})"
            ])
        , "endif()"
        , "target_compile_definitions(" <> targetName <> " PUBLIC -DNXA_PERSISTENCE_CURRENT_REKORD_BUDDY_SCHEMA_VERSION=V" <> pretty (Con.configurationVersion config) <> ")"
        , "target_link_libraries(" <> targetName <> " Base Persistence SQLiteCpp)"
        , "target_include_directories(" <> targetName <> " PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/Include ${CMAKE_CURRENT_SOURCE_DIR}/Common)"
        ])
 where entities = Con.configurationEntities config
       targetName = pretty ( mconcat (Con.configurationNamespaces config)) <> "Persistence"
       sortedEntities = sortBy (compare `on` stringFormInclude (Con.configurationEntities config)) (Map.elems entities)
       makeCppName x =
                 "Objects/V" <>pretty (Con.configurationVersion config) <> "/" <> pretty (entityName (ultimateParent entities x))  <> "/Persistent" <> pretty (entityName x) <> ".cpp"

renderVersionFile :: Con.Configuration -> String
renderVersionFile config = nxaRender $
   vsep [ copyright
        , pragmaOnce
        , includeSchema config "MetaMacroAllTypes.ipp"
        , namespace 0 False mempty
            [ assign "constexpr uinteger16 version" (pretty (Con.configurationVersion config))
            , assign "constexpr uinteger16 readableSinceVersion" (pretty $ Con.minVersion config)
            , callS "static_assert" ["readableSinceVersion"/<=/"version" /&&/ "version" />/ pretty (0 :: Int), literalString ("Schema Version" <+>  pretty (Con.configurationVersion config) <+> "must be greater than readableSinceVersion")]
         ]
        ]

-- | Produce a list of names and bodies. each snd should be put in a file named after fst.
objectiveCWrapperHeader :: Con.Configuration -> Entity -> Doc ann
objectiveCWrapperHeader config ent =
   vsep [ copyright
        , pragmaOnce
        , vcat [ generateInclude ObjcMainInclude config ent
               , vcat (map (\rn -> objcImport (nxAS <> pretty rn <> ".h")) (nub (map relationshipDest relns)))
               , includeRoot config ((pretty $ head (Con.configurationNamespaces config)) <> "Schema.hpp")
               , generateInclude PublicInclude config ent
               ]
        , interfaceProperty
              (nxAS <> fieldName ent)
              (sharedPtrOf (nxArb config ("Persistent" <> fieldName ent)))
        ]
 where
   attrs = orderedNonTransientAttributes ent
   relns = Map.elems $ entityRelationships ent
   interfaceProperty nam localPt =
       objcInterface (nam <+> parens "CppCategory")
         [ statement ("@property" <+> parenCommaList [] <+> localPt <+> memberName)
         , statement (objcClassMethodDecl (nxATypeS config) ["objectType"])
         , pragmaMark "Relationships"
         , vcat (map (relationshipPropDecl config ent) relns)
         , pragmaMark "Attributes"
         , vcat (map (attributePropDecl config ent) attrs)
         ]
