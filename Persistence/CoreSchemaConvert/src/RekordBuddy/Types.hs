{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module RekordBuddy.Types where

import Data.Text (Text, toUpper)
import qualified Data.Text as T
import qualified Data.Map.Lazy as Map
import qualified Data.Char as C
import Data.Maybe (fromMaybe)
import Data.Text.Prettyprint.Doc
import Data.List (sort)
import RekordBuddy.Helpers (dtext, sharedPtrOf, uniquePtrOf, template, (/::/), void, reference)

type EntityMap = Map.Map Text Entity
type FetchRequestMap = Map.Map Text FetchRequest
type AttributeMap = Map.Map Text Attribute
type RelationshipMap = Map.Map Text Relationship

newtype ManyToMany = ManyToMany {leftAndRight :: (Relationship, Relationship) }
  deriving Show

schemaType :: Doc ann
schemaType = "RBSchema"

relationshipAndInverse :: EntityMap -> Relationship -> ManyToMany
relationshipAndInverse ents r = ManyToMany (r, fromMaybe (error "M2M requires inverse") (inverseRelationship r ents))

leftRelation, rightRelation :: ManyToMany -> Relationship
leftRelation = fst.leftAndRight
rightRelation = snd.leftAndRight

mirrorRelation :: ManyToMany -> ManyToMany
mirrorRelation r =
   let (a,b) = leftAndRight r
   in ManyToMany (b,a)

-- | Represent a Core Data schema document internally
data Model = Model
   { modelEntities      :: EntityMap
   , modelFetchRequests :: FetchRequestMap
   }
 deriving Show

-- | Represent any core data entity inside the model
data Entity = Entity
   { entityName                  :: Text
   , entityParentName            :: Maybe Text
   , entityRepresentedClassName  :: Text
   , entitySyncable              :: Bool
   , entityIsAbstract            :: Bool
   , entityAttributes            :: AttributeMap
   , entityRelationships         :: RelationshipMap
   }
 deriving (Show, Eq, Ord)

-- | Each core data attribute has a type, which is canonically tied to NS types but which we will simulate
data AttributeType = AttributeInteger Int
                   | AttributeDate
                   | AttributeDuration
                   | AttributeDecimal
                   | AttributeBinary
                   | AttributeBoolean
                   | AttributeString
                   | AttributeTransformable (Maybe Text)
                   | AttributeNone
 deriving (Show, Eq, Ord)

-- | Core Data attributes
data Attribute = Attribute
   { attributeName      :: Text
   , attributeType      :: AttributeType
   , attributeSyncable  :: Bool
   , attributeOptional  :: Bool
   , attributeTransient :: Bool
   , attributeIndexed   :: Bool
   , minValueString     :: Maybe Text
   }
 deriving (Show, Eq, Ord)


-- | Whether to cascade or nullify a relationship upon delete
data RelationshipDeleteRule = Cascade | Nullify
 deriving (Show, Ord, Eq)

-- | A Relationship relates two entities
data Relationship = Relationship
   { relationshipDest        :: Text
   , relationshipName        :: Text
   , relationshipOptional    :: Bool
   , relationshipToMany      :: Bool
   , relationshipOrdered     :: Bool
   , relationshipRule        :: RelationshipDeleteRule
   , relationshipInverseName :: Maybe Text
   , relationshipSyncable    :: Bool
   , relationshipMin         :: Maybe Integer
   , relationshipMax         :: Maybe Integer
   }
 deriving (Show, Ord, Eq)

-- | Represent a core data fetch request
data FetchRequest = FetchRequest
   { fetchName        :: Text
   , fetchEntityName  :: Text
   , fetchPredicate   :: Text
   , fetchLimit       :: Integer
   , fetchSubentities :: Bool
   }
 deriving (Show, Eq)

data Scope = NoNamespace | Namespaced | SchemaScoped | RbScoped | NoScoping
 deriving (Show, Eq)

data Visibility = Public | Internal
   deriving (Show, Eq)

data ValueOrIndex = ItemByValue | ItemByIndex | ItemsByValue | ItemsByIndex | ItemsByObjectID | ObjectIDByIndex | AllItems | AllObjectIDs | AllObjectIDsOfType

makeRight :: ValueOrIndex -> Text -> Text
makeRight ItemByValue d        = T.append d "Item"
makeRight ItemByIndex d        = T.append d "ItemAtIndex"
makeRight ObjectIDByIndex d    = T.append (T.append "objectIDFor" (transformUpperCaseInitial d)) "ItemAtIndex"
makeRight ItemsByValue d       = T.append d "Items"
makeRight ItemsByIndex d       = T.append d "ItemsAtIndex"
makeRight ItemsByObjectID d    = T.append d "ItemsByObjectID"
makeRight AllItems d           = d
makeRight AllObjectIDs d       = T.append d "ObjectIDs"
makeRight AllObjectIDsOfType d = T.append d "ObjectIDsOfType"

class AOR a where
   rawFieldName :: a -> Text
   fieldOptional :: a -> Bool
   cdName, fieldName, instanceName, entName, colName :: a -> Doc ann
   baseName, titleCaseName :: ValueOrIndex -> a -> Doc ann

   fieldType :: Doc ann -> Visibility -> Scope -> a -> Doc ann
   fieldName x     = dtext (rawFieldName x)
   instanceName  x  = baseName AllItems x <> "Attribute"
   titleCaseName c x = dtext $ makeRight c $ transformUpperCaseInitial (rawFieldName x)
   baseName c x      = dtext $ makeRight c $ (transformBaseName (rawFieldName x))
   cdName x        = "Z" <> dtext (toUpper (rawFieldName x))
   entName x       = "ent_" <> fieldName x
   colName x       = "col_" <> fieldName x
   rawTypeName :: a -> Doc ann
   rawTypeName x =  internalType (rawFieldName x)

instance AOR Entity where
   rawFieldName = entityName
   fieldOptional _ = False
   fieldType _ Internal NoNamespace e = internalFieldName Internal e
   fieldType _ Internal Namespaced e = nxA (internalFieldName Internal e)
   fieldType _ Public NoScoping e = internalFieldName Public e
   fieldType c Public SchemaScoped e = "Type" /::/ fieldType c Public NoScoping e
   fieldType c Public RbScoped e = schemaType /::/ fieldType c Public SchemaScoped e
   fieldType c Public s e = nxaNS s (c /::/ fieldType c Public RbScoped e)
   fieldType _ v s e = error $ "fieldType: Can't generate name for " ++ T.unpack (rawFieldName e) ++ " that is " ++ show v ++ " " ++ show s

instance AOR Relationship where
   rawFieldName = relationshipName
   fieldOptional = relationshipOptional
   rawTypeName rel = internalType (relationshipDest rel)
   instanceName r | relationshipToMany r = "many" <> titleCaseName AllItems r
   instanceName r | relationshipOptional r = baseName AllItems r <> "OptionalId"
   instanceName r = baseName AllItems r <> "Id"
   fieldType c Internal s rel | relationshipToMany rel =
      uniquePtrOf
         (template (nxaNS s "PersistentRelationship")
            [sharedPtrOf (internalType (relationshipDest rel)), nxaNS s (c /::/schemaType), "LocalType"])
   fieldType c Public   s rel | relationshipToMany rel =
      reference
         (template (nxaNS s "PersistentRelationship")
            [sharedPtrOf (internalType (relationshipDest rel)), nxaNS s (c /::/schemaType), "LocalType"])
   fieldType _ _ s rel =
      optionalAOR s rel (nxaNS s "RBSchema::ObjectID")

instance AOR Attribute where
   rawFieldName = attributeName
   fieldOptional = attributeOptional
   fieldType _ _ _   Attribute{attributeType=AttributeNone}              = void
   fieldType _ _ s a@Attribute{attributeType=AttributeDate}              = optionalAOR s a (nxaNS s "Time")
   fieldType _ _ s a@Attribute{attributeType=AttributeDecimal}           = optionalAOR s a (nxaNS s "DecimalNumber")
   fieldType _ _ s a@Attribute{attributeType=AttributeString}            = optionalAOR s a (nxaNS s "String")
   fieldType _ _ s a@Attribute{attributeType=AttributeDuration}          = optionalAOR s a (nxaNS s "Duration")
   fieldType _ _ s a@Attribute{attributeType=AttributeBinary}            = optionalAOR s a (nxaNS s "Blob")
   fieldType _ _ s a@Attribute{attributeType=AttributeTransformable _ }  = optionalAOR s a (nxaNS s "Blob")
   fieldType _ _ s a@Attribute{attributeType=AttributeBoolean}           = optionalAOR s a (nxaNS s "boolean")
   fieldType _ _ s a@Attribute{attributeType=AttributeInteger i }        = optionalAOR s a (nxaNS s ("integer" <> pretty i))

internalFieldName :: AOR a => Visibility -> a -> Doc ann
internalFieldName Public e =  dtext (rawFieldName e)
internalFieldName Internal e = internalType (rawFieldName e)

internalType :: Text -> Doc ann
internalType t = "Persistent" <> dtext t

nxAT :: Text
nxAT = "NxA"

nxAS :: Doc ann
nxAS = dtext nxAT

nxA :: Doc ann -> Doc ann
nxA = (nxAS /::/)

nxaNS :: Scope -> Doc ann -> Doc ann
nxaNS Namespaced = nxA
nxaNS _ = id

transformBaseName :: Text -> Text
transformBaseName t =
   let pre = T.take 2 t
       suf = T.drop 2 t
   in if pre == "p_" then suf else t

transformUpperCaseInitial :: Text -> Text
transformUpperCaseInitial t =
   case T.uncons (transformBaseName t) of
    Just (l, rest) -> T.cons (C.toUpper l) rest
    Nothing -> "___unnamed___"

baseNameStr :: Text -> Doc ann
baseNameStr t = dtext (transformBaseName t)

ucaseBaseNameStr :: Text -> Doc ann
ucaseBaseNameStr t = dtext (transformUpperCaseInitial t)

optionalType :: Scope -> Doc ann
optionalType f = nxaNS f "Optional"

optionalOf :: Scope -> Doc ann -> Doc ann
optionalOf f t = template (optionalType f) [t]

optionalAOR :: AOR a => Scope -> a -> Doc ann -> Doc ann
optionalAOR f a body = if fieldOptional a then optionalOf f body else body

parentEntity :: EntityMap -> Entity -> Maybe Entity
parentEntity ents e = entityForName (entityParentName e) ents

-- | True if q is a p, false otherwise
isKindOf :: EntityMap -> Entity -> Entity -> Bool
isKindOf _es q p | p == q = True
isKindOf es q p =
   case parentEntity es q of
         Just pq -> isKindOf es pq p
         Nothing -> False

-- | Given some entity, find another entity, if any that is a child and not abstract
maybeChildType :: EntityMap -> Entity -> Maybe Entity
maybeChildType es e =
   let filtered = filter (\ec -> not (entityIsAbstract ec) && isKindOf es ec e) (Map.elems es)
   in case sort filtered of
         [] -> Nothing
         (a:_) -> Just a

-- | Given some entity and a map of entities, will find the ultiumate parent of the entity (that with no other parent)
ultimateParent :: EntityMap -> Entity -> Entity
ultimateParent _ents e@Entity{entityParentName=Nothing} = e
ultimateParent ents Entity{entityParentName=Just p} =
   case Map.lookup p ents of
      Just pe -> ultimateParent ents pe
      Nothing -> error "Failed to find parent in map"

-- Given a relationship and the EntityMap, maybe produce the relationship that is the inverse of r
inverseRelationship :: Relationship -> EntityMap -> Maybe Relationship
inverseRelationship Relationship{..} es = do
   irn <- relationshipInverseName
   ie <- Map.lookup relationshipDest es
   Map.lookup irn (entityRelationships ie)

entityOfRelationshipDestination :: Relationship -> EntityMap -> Maybe Entity
entityOfRelationshipDestination Relationship{..} = Map.lookup relationshipDest


entityForName :: Maybe Text -> EntityMap -> Maybe Entity
entityForName a ents = do
   ja <- a
   Map.lookup ja ents

