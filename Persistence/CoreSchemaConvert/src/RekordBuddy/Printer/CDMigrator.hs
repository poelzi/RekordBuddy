{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module RekordBuddy.Printer.CDMigrator where
import           Data.Text.Prettyprint.Doc
import qualified Data.Map.Lazy as Map
import Data.Maybe (isJust)
import           RekordBuddy.Types
import qualified RekordBuddy.Configuration as Con
import           RekordBuddy.Helpers
import           RekordBuddy.Printer.NxA
import Data.List (foldl')

entityID :: Doc ann
entityID = "EntityID"

entityType :: Doc ann
entityType = "EntityType"

objectID :: Doc ann
objectID = "ObjectID"

entTypeMap, objTypeMap, objectIDMap, entityIDMap :: Doc ann
entTypeMap = entityType <> "Map"
objTypeMap = "SchemaType" <> "Map"
objectIDMap = objectID <> "Map"
entityIDMap = entityID <> "Map"


migratorDoc :: Con.Configuration -> Entity -> Doc ann
migratorDoc config e =
   foldl' (%%) copyright
      [ vcat
         [ includeSchema config "V1CocoaMigration.hpp"
         , includeRoot config ((dtext $ head (Con.configurationNamespaces config)) <> "Schema.hpp")
         , include "Persistence/Persistence.hpp"
         , includeSystem "SQLiteCpp/SQLiteCpp.h"
         , includeSystem "memory"
         ]
      , vcat
         [ usingNamespace "std"
         , usingNamespace nxAS
         , usingNamespace (nxA (joinNamespaces (Con.configurationNamespaces config)))
         ]
      , vcat
         [ statement $ "extern" <+> objectIDMap <+> "oidmap"
         , statement $ "extern" <+> entityIDMap <+> "eidmap"
         , statement $ "extern" <+> objTypeMap  <+> "typeMap"
         , statement $ "extern" <+> entTypeMap  <+> "inverseTypeMap"
         , statement $ "extern" <+> sharedPtrOf ("SQLite"/::/"Database") <+> "sourceDatabase"
         , statement $ "extern" <+> sharedPtrOf (template "PersistentContext" [schemaType]) <+> "context"
         , statement $ "extern" <+> methodDecl "maintainReverseLookupMaps" []
                                       [persistType <+> "type", objectID <+> "objectID", entityID <+> "entityID"] "void"
         ]
      , migrateEntityFromRow config e
      , migrateEntityByID config e
      , migrateEntitiesOfType config e
      ]

sqlNameForEntity :: EntityMap -> Entity -> Doc ann
sqlNameForEntity ents e =  cdName (ultimateParent ents e)


migrateEntityByID :: Con.Configuration -> Entity -> Doc ann
migrateEntityByID config e =
   methodDef (Con.nesting config) (nxA "migrate" <> fieldName e) []
         [optionalOf NoNamespace entityID <+> "optionalEntityID", migrationCallbackDecl]
         (optionalOf NoNamespace (sharedPtrOf (fieldType (joinNamespaces (Con.configurationNamespaces config)) Internal NoNamespace e)))
      [ guardP (Con.nesting config) (negateP "optionalEntityID") [returnS $ braceList []]
      , autoInit "entityID" (deref "optionalEntityID")
      , statement (optionalOf NoNamespace (sharedPtrOf (fieldType (joinNamespaces (Con.configurationNamespaces config)) Internal NoNamespace e)) <+> "object")
      , statement (("SQLite"/::/"Statement") <+> initialize "entity"
         [deref "sourceDatabase", literalString ("SELECT * FROM"<+> sqlNameForEntity (Con.configurationEntities config) e <+>" WHERE z_pk = :z_pk")])
      , callS "entity.bind" [literalString ":z_pk", "entityID"]
      , whileP (Con.nesting config) (call "entity.executeStep" [])
         [ autoInit "persistType" (call "entity.getColumn" [literalString "Z_ENT"])
         , autoInit "migrateFunction" ("migrationFunctionsForType" <> brackets ("typeMap" <> brackets (call "persistType.getInt64" [])))
         , autoInitTestScope (Con.nesting config) "migrated" id (call "migrateFunction" ["entity", "progressCallback"])
            [ autoInitTestScope (Con.nesting config) "maybeCasted" id (call (template "dynamic_pointer_cast" [fieldType (joinNamespaces (Con.configurationNamespaces config)) Internal NoNamespace e]) [deref "migrated"])
               [ assign "object" "maybeCasted"]
            ]
         ]
      , returnS "object"
      ]

migrationCallbackDecl :: Doc ann
migrationCallbackDecl = "const" <+> template "function" ["void()"] <+> "&progressCallback"

migrateEntitiesOfType :: Con.Configuration -> Entity -> Doc ann
migrateEntitiesOfType config e =
   methodDef (Con.nesting config) (nxA "migrate" <> fieldName e) []
         [migrationCallbackDecl]
         "void"
      [ variableDef "SQLite::Statement" "entity" [deref "sourceDatabase", literalString
            ("SELECT * FROM" <+> sqlNameForEntity (Con.configurationEntities config) e <+> "WHERE z_ent = :z_ent")]
      , autoInit "entityType" (call "inverseTypeMap.at" [persistType /::/ fieldName e])
      , callS "entity.bind" [literalString ":z_ent", "entityType"]
      , whileP (Con.nesting config) (call "entity.executeStep" [])
         [ callS ("migrate" <> fieldName e) ["entity", "progressCallback"]
         ]
      ]

migrateEntityFromRow :: Con.Configuration -> Entity -> Doc ann
migrateEntityFromRow config e =
   methodDef (Con.nesting config) (nxA "migrate" <> fieldName e) []
         ["SQLite"/::/"Statement&" <+> "entity", migrationCallbackDecl]
         (optionalOf NoNamespace (sharedPtrOf "PersistentObject"))
      [vsep
         [ vcat
            [ assign (entityID <+> "entityID") (call "entity.getColumn" [literalString "Z_PK"] /./ call "getInt64" [])
            , assign (entityType <+> "entType") (call "entity.getColumn" [literalString "Z_ENT"] /./ call "getInt64" [])
            , assign (entityType <+> "currentType") ("inverseTypeMap" <> brackets (fieldType (joinNamespaces (Con.configurationNamespaces config)) Public RbScoped e))
            , callS "NXA_ASSERT_TRUE" [call (schemaType /::/ "typeIs") ["typeMap[entType]", fieldType (joinNamespaces (Con.configurationNamespaces config)) Public RbScoped e]]
            ]
         , vcat
            [ statement (optionalOf NoNamespace (sharedPtrOf (fieldType (joinNamespaces (Con.configurationNamespaces config)) Internal NoNamespace e)) <+> "maybeObject")
            , autoInit "oidmapIterator" ("oidmap"/./call "find" [call "make_pair" ["entityID", "currentType"]])
            , ifP (Con.nesting config) ("oidmapIterator" /!=/ "oidmap"/./call "end" [])
               [ assign "maybeObject" ("context"/->/ call (template "fetchObject" [fieldType (joinNamespaces (Con.configurationNamespaces config)) Internal NoNamespace e]) ["oidmapIterator"/->/"second"])
               , callS "NXA_ASSERT_TRUE" ["maybeObject"]
               , guardP (Con.nesting config) (call "doneOrStartedObjectIds.find"   ["oidmapIterator"/->/"second"] /!=/ call "doneOrStartedObjectIds.end" [])
                  [returnS (call "sharedDowncast" ["maybeObject"])]
               ]
               (Just [ assign "maybeObject" ("context"/->/call (template "createObject" [fieldType (joinNamespaces (Con.configurationNamespaces config)) Internal NoNamespace e]) [])
                     , callS "progressCallback" []
                     ])
            ]
         , vcat
            [ callS "NXA_ASSERT_TRUE" ["maybeObject"]
            , autoInit "object" (deref "maybeObject")
            , callS "maintainReverseLookupMaps" ["typeMap[currentType]", "object" /->/ call "objectID" [], "entityID"]
            , migrateParent e
            , callS "doneOrStartedObjectIds.insert" ["object" /->/ call "objectID" []]
            ]
         , vsep (map (attributeDecodeForMigration (Con.nesting config)) (orderedNonTransientAttributes e))
         , vsep (map (relationshipDecodeForMigration config e) (Map.elems (entityRelationships e)))
         , returnS (call "sharedDowncast" ["object"])
         ]
      ]

migrateParent :: Entity -> Doc ann
migrateParent Entity{entityParentName = Nothing} = mempty
migrateParent Entity{entityParentName = Just x} = callS ("migrate" <> dtext x) ["entity", "progressCallback"]

easyAssign :: AOR a => a -> (Doc ann -> Doc ann) -> Doc ann
easyAssign a f = assign (entName a) (f (colName a))

-- | Given an attribute, provide lines of statements that will convert the column expression named 'colName' into a local variable named entName
decodeAttribute :: Attribute -> [Doc ann]
decodeAttribute a@Attribute{attributeType = AttributeInteger 64}      = [easyAssign a (/./ call "getInt64" [])]
decodeAttribute a@Attribute{attributeType = AttributeInteger _}       = [easyAssign a (/./ call "getInt" [])]
decodeAttribute a@Attribute{attributeType = AttributeDate}            = [easyAssign a (/./ call "getInt64" [])]
decodeAttribute a@Attribute{attributeType = AttributeDuration}        = [easyAssign a (\x -> call "Duration::fromFlicks" [x /./ call "getInt64" []])]
decodeAttribute a@Attribute{attributeType = AttributeDecimal}         = [easyAssign a (\x -> "decimal" <> braceList [x /./ call "getDouble" []])]
decodeAttribute a@Attribute{attributeType = AttributeBoolean}         = [easyAssign a (\x -> x /./ call "getInt" [] /!=/ "0")]
decodeAttribute a@Attribute{attributeType = AttributeString}          = [easyAssign a (\x -> "String" <> braceList [x /./ call "getText" []])]
decodeAttribute a@Attribute{attributeType = AttributeBinary}          = [easyAssign a (\x -> call "Blob::withMemoryAndSize" [call "static_cast<const byte*>" [x /./ call "getBlob" []], x /./ call "getBytes" []])]
decodeAttribute a@Attribute{attributeType = AttributeTransformable _} = [easyAssign a (\x -> call "Blob::withMemoryAndSize" [call "static_cast<const byte*>" [x /./ call "getBlob" []], x /./ call "getBytes" []])]
decodeAttribute a@Attribute{attributeType = AttributeNone} = error ("unsupported type on " ++ show a )

entType :: Attribute -> Doc ann
entType Attribute{attributeType = AttributeInteger i}       = "integer" <> pretty i
entType Attribute{attributeType = AttributeDuration}        = "Duration"
entType Attribute{attributeType = AttributeDate}            = "timestamp"
entType Attribute{attributeType = AttributeDecimal}         = "decimal"
entType Attribute{attributeType = AttributeBinary}          = "Blob"
entType Attribute{attributeType = AttributeBoolean}         = "boolean"
entType Attribute{attributeType = AttributeString}          = "String"
entType Attribute{attributeType = AttributeTransformable _} = "Blob"
entType Attribute{attributeType = AttributeNone} = "void"

assertOrCheckAttribute :: Int -> Attribute -> Doc ann
assertOrCheckAttribute nnesting a | fieldOptional a =
   guardP nnesting (negateP (colName a /./ call "isNull" [])) (decodeAttribute a)
assertOrCheckAttribute _nesting a =
   callS "NXA_ASSERT_FALSE" [colName a /./ call "isNull" []] %% vcat (decodeAttribute a)

assertOrCheckRelation :: Con.Configuration -> Relationship -> Doc ann
assertOrCheckRelation config r@Relationship{..} | fieldOptional r =
   guardP (Con.nesting config) (negateP (colName r /./ call "isNull" []))
      [ assign (entName r) (call ("migrate" <> dtext relationshipDest) [colName r /./ call "getInt64" [], "progressCallback"]) ]
assertOrCheckRelation _config r@Relationship{..} =
    vcat [
      callS "NXA_ASSERT_FALSE" [colName r /./ call "isNull" []]
   %% autoInit ("migrated" <> entName r) (call ("migrate" <> dtext relationshipDest) [colName r /./ call "getInt64" [], "progressCallback"])
   , callS "NXA_ASSERT_TRUE" ["migrated" <> entName r]
   , assign (entName r) (deref ("migrated" <> entName r))
         ]

attributeDecodeForMigration :: Int -> Attribute -> Doc ann
attributeDecodeForMigration nnesting a@Attribute{..} = vcat
   [ statement ((if attributeOptional then optionalOf NoNamespace (entType a) else entType a) <+> entName a)
   , autoInit (colName a) ("entity" /./ call "getColumn" [literalString (cdName a)])
   , assertOrCheckAttribute nnesting a
   , callS ("object"/->/cppSetterNameP AllItems a) [entName a]
   ]

relationshipDecodeForMigration :: Con.Configuration -> Entity -> Relationship -> Doc ann
relationshipDecodeForMigration config _e r@Relationship{..} | not relationshipToMany = vcat
   [ pragmaMark "one-to-one migration relationship"
   , autoInit (colName r) ("entity" /./ call "getColumn" [literalString (cdName r)])
   , statement (
      (if fieldOptional r
         then optionalOf NoNamespace (sharedPtrOf (internalType relationshipDest))
         else sharedPtrOf (internalType relationshipDest)) <+> entName r)
   , assertOrCheckRelation config r
   , callS ("object"/->/cppSetterNameP AllItems r) [entName r]
   ]

relationshipDecodeForMigration config e r |
       relationshipToMany r &&
       fmap relationshipToMany (inverseRelationship r (Con.configurationEntities config)) == Just True = vcat
   [ autoInitTestScope (Con.nesting config) name id (call "decodeManyToManyForMigration" [fieldType (joinNamespaces $ Con.configurationNamespaces config) Internal NoNamespace e, literalString (fieldName r)])
      [ autoInit "query" ("String"/::/ call "stringWithFormat" [literalString "SELECT %s FROM %s WHERE %s = :condition ORDER BY %s", get 2, get 0, get 1, get 3])
      , statement ("SQLite"/::/"Statement"<+>"statement" <> braceList [deref "sourceDatabase", call ("query"/./"asUTF8") []])
      , callS "statement.bind" [literalString ":condition", "entityID"]
      , whileP (Con.nesting config) (call "statement.executeStep" [])
         [ autoInitTestScope (Con.nesting config) "otherEntityID" (\x -> negateP (call (x /./ "isNull") [])) (call "statement.getColumn" [pretty (0 :: Int)])
            [ autoInitTestScope (Con.nesting config) "result" id (call ("migrate" <> dtext (relationshipDest r)) [braceList [call "otherEntityID.getInt64" []], "progressCallback"])
               [ callS ("object"/->/cppAdderNameP ItemByValue r) [deref "result"]
               ]
            ]
         ]
      ]
   ]
 where name = "m2m" <> fieldName r
       get :: Int -> Doc ann
       get i = stdGet (pretty i) (deref name)

relationshipDecodeForMigration _config _e _r =
   lineComment "one-to-many migration relationship handled by inverse o-2-o"

