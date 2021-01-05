{-# LANGUAGE OverloadedStrings, RankNTypes, RecordWildCards, NamedFieldPuns #-}
module RekordBuddy.CoreData.Parser(readSchema) where

import Prelude hiding (readFile)
import Control.Monad (unless)
import Data.Maybe (isJust, catMaybes)
import Data.Text (Text)
import Data.List (partition)
import qualified Data.Text as T
import qualified Data.Map.Lazy as Map
import Text.XML
import Text.XML.Cursor
import RekordBuddy.Types

readSchema :: FilePath -> IO (Either String (Map.Map Text Entity))
readSchema fp = do
   doc <- readFile def fp
   return . Right $ Map.fromList (map processNodeToEntity (entityNodePath doc))

entityNodePath :: Document -> [Cursor]
entityNodePath d = fromDocument d $// element "entity"

relevantChildren :: [Node] -> [Element]
relevantChildren [] = []
relevantChildren (NodeElement e : ns) | elementName e == "attribute" || elementName e == "relationship" = e : relevantChildren ns
relevantChildren (_:ns) = relevantChildren ns

decodeAttributeType :: Maybe Text -> Text -> AttributeType
decodeAttributeType _ "String"        = AttributeString
decodeAttributeType _ "Integer 8"     = AttributeInteger 8
decodeAttributeType _ "Integer 16"    = AttributeInteger 16
decodeAttributeType _ "Integer 32"    = AttributeInteger 32
decodeAttributeType _ "Integer 64"    = AttributeInteger 64
decodeAttributeType _ "Decimal"       = AttributeDecimal
decodeAttributeType _ "Binary"        = AttributeBinary
decodeAttributeType _ "Boolean"       = AttributeBoolean
decodeAttributeType _ "Date"          = AttributeDate
decodeAttributeType _ "Duration"      = AttributeDuration
decodeAttributeType (Just "") "Transformable" = AttributeTransformable Nothing
decodeAttributeType t "Transformable" = AttributeTransformable t
decodeAttributeType _ t               = error $ "Encountered unknown type" ++ T.unpack t

decodeBoolean :: Text -> Bool
decodeBoolean str | str == "YES" = True
decodeBoolean _                  = False

decodeReadable :: forall a. Read a => Text -> a
decodeReadable str = read (T.unpack str)

decodeRule :: Text -> RelationshipDeleteRule
decodeRule "Cascade" = Cascade
decodeRule "Nullify" = Nullify
decodeRule "No Action" = error "No Action delete rule"

defFls :: Maybe Bool -> Maybe Bool
defFls (Just x) = Just x
defFls Nothing  = Just False

defNone :: Maybe AttributeType -> Maybe AttributeType
defNone (Just x) = Just x
defNone Nothing  = Just AttributeNone

elementToAttribute :: Element -> Maybe (Text, Attribute)
elementToAttribute (Element "attribute" attrs _) = do
   attributeName           <- Map.lookup "name" attrs
   let attributeTransformer = Map.lookup "valueTransformerName" attrs
   let minValueString       = Map.lookup "minValueString" attrs
   attributeSyncable       <-           decodeBoolean <$> Map.lookup "syncable" attrs
   attributeOptional       <- defFls  $ decodeBoolean <$> Map.lookup "optional" attrs
   attributeIndexed        <- defFls  $ decodeBoolean <$> Map.lookup "indexed" attrs
   attributeTransient      <- defFls  $ decodeBoolean <$> Map.lookup "transient" attrs
   attributeType           <- defNone $ decodeAttributeType attributeTransformer <$> Map.lookup "attributeType" attrs
   return (attributeName, Attribute {..})

elementToRelationship :: Element -> Maybe (Text, Relationship)
elementToRelationship (Element "relationship" attrs _) = do
   relationshipName           <- Map.lookup "name" attrs
   relationshipOptional       <- defFls $ decodeBoolean  <$> Map.lookup "optional" attrs
   relationshipToMany         <- defFls $ decodeBoolean  <$> Map.lookup "toMany" attrs
   relationshipOrdered        <- defFls $ decodeBoolean  <$> Map.lookup "ordered" attrs
   relationshipRule           <-          decodeRule     <$> Map.lookup "deletionRule" attrs
   relationshipSyncable       <-          decodeBoolean  <$> Map.lookup "syncable" attrs
   let relationshipMin         =          decodeReadable <$> Map.lookup "minCount" attrs
   let relationshipMax         =          decodeReadable <$> Map.lookup "maxCount" attrs
   relationshipDest           <-                             Map.lookup "destinationEntity" attrs
   let relationshipInverseName =                             Map.lookup "inverseName" attrs
   return (relationshipName, Relationship {..})

elementToEntity :: Element -> Maybe Entity
elementToEntity (Element "entity" attrs children) = do
   entityName                 <- Map.lookup "name" attrs
   let entityParentName        = Map.lookup "parentEntity" attrs
   entityRepresentedClassName <- Map.lookup "representedClassName" attrs
   entitySyncable             <- defFls $ decodeBoolean <$> Map.lookup "syncable" attrs
   entityIsAbstract           <- defFls $ decodeBoolean <$> Map.lookup "isAbstract" attrs
   let (attribs, rels)         = partition (("attribute" ==) . elementName) (relevantChildren children)
   let (entityAttributesM, e1) = partition isJust $ map elementToAttribute attribs
   let (entityRelationshipsM, e2) = partition isJust $ map elementToRelationship rels
   unless (null e1) Nothing
   unless (null e2) Nothing
   let entityAttributes        = Map.fromList . filter (("cppObjectID" /=) . fst) $ catMaybes entityAttributesM
   let entityRelationships     = Map.fromList $ catMaybes entityRelationshipsM
   return Entity {..}
elementToEntity _ = Nothing

processNodeToEntity :: Cursor -> (Text, Entity)
processNodeToEntity a = case node a of
                           NodeElement e -> case elementToEntity e of
                                                 Just elm -> (entityName elm, elm)
                                                 Nothing -> error $ "failed to parse node " ++ err e
                           _ -> error "Not a node"
  where err Element{elementAttributes} = show (elementAttributes Map.! "name")


