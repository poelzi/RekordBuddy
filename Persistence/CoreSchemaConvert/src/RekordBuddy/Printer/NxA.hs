{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module RekordBuddy.Printer.NxA where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc
import           Data.Function(on)
import           Data.List(sortBy, nub)
import qualified Data.Char as C
import qualified Data.Map.Lazy as Map
import           Data.Maybe(catMaybes)
import           RekordBuddy.Types
import           RekordBuddy.Helpers
import qualified RekordBuddy.Configuration as Con

joinNamespaces :: [Text] -> Doc ann
joinNamespaces as = dtext $ T.intercalate "::" as

newObject :: AOR a => Con.Configuration -> a -> Doc ann
newObject c e = call (nxA "wrapperFor") [cppCreate]
 where cppCreate = call (template ("context" /./ "cppContext" /->/ "createObject")
                                  [nxaNS Namespaced (joinNamespaces (Con.configurationNamespaces c)) /::/ rawTypeName e]) []

ucaseRelName :: Relationship -> Doc ann
ucaseRelName Relationship{..} = ucaseFirst . T.unpack $ relationshipName

includeSchema, includeRoot :: Con.Configuration -> Doc ann -> Doc ann
includeSchema cfg str = includeRoot cfg ("V" <> pretty (Con.configurationVersion cfg) <> "/" <> str)
includeRoot cfg str   = includeSystem (pretty (mconcat (Con.configurationNamespaces cfg)) <> "Persistence/" <> str)

-- Test-and-deref only if optional, otherwise directly use
optionalBool :: Int -> Bool -> Doc ann -> (Doc ann -> Doc ann) -> (Doc ann -> [Doc ann]) -> Doc ann
optionalBool anesting opt doc qf body =
   if opt
   then guardP anesting (qf doc) (body (parens (deref doc)))
   else vcat (body doc)

optionalCond' :: AOR a => Con.Configuration -> a -> (a -> Doc ann) -> (Doc ann -> Doc ann) -> (Doc ann -> [Doc ann]) -> Doc ann
optionalCond' config a name = optionalBool (Con.nesting config) (fieldOptional a) (name a)

optionalCond :: AOR a => Con.Configuration -> a -> (a -> Doc ann) -> (Doc ann -> [Doc ann]) -> Doc ann
optionalCond config a name = optionalCond' config a name id

wrapperSetterNameP, wrapperGetterNameP, wrapperAdderNameP, wrapperRemoverNameP :: AOR a => a -> Doc ann
wrapperGetterNameP a  = dtext (rawFieldName a)
wrapperAdderNameP a   = "add" <> ucaseFirst (T.unpack $ rawFieldName a)
wrapperRemoverNameP a = "remove" <> ucaseFirst (T.unpack $ rawFieldName a)
wrapperSetterNameP a  = "set" <> ucaseFirst (T.unpack $ rawFieldName a)


cppCounterNameP, cppFetchByNameP, cppCountNameP, cppRemoveAllNameP  :: AOR a => a -> Doc ann
cppCountNameP a     = "numberOf" <> titleCaseName AllItems a
cppRemoveAllNameP a = "removeAll" <> titleCaseName AllItems a
cppCounterNameP a   = "count" <> titleCaseName AllItems a
cppFetchByNameP a   = "fetchBy" <> titleCaseName AllItems a

cppSetterNameP, cppGetterNameP, cppOrderNameP, cppGetObjectID, cppAdderNameP, cppRemoverNameP, cppLoaderNameP :: AOR a => ValueOrIndex -> a -> Doc ann
cppOrderNameP c a   = "order" <> titleCaseName c a
cppAdderNameP c a   = "add" <> titleCaseName c a
cppRemoverNameP c a = "remove" <> titleCaseName c a
cppGetObjectID c a  = "objectIDFor" <> titleCaseName c a
cppSetterNameP c a  = "set" <> titleCaseName c a
cppGetterNameP c a  = baseName c a
cppLoaderNameP c a    = "load" <> titleCaseName c a

setDefaultForRelation :: Relationship -> Doc ann
setDefaultForRelation Relationship{..} | relationshipOrdered  = "orderedSet"
setDefaultForRelation Relationship{..}                        = "set"

setTypeForRelation :: Bool -> Relationship -> Doc ann
setTypeForRelation True Relationship{..} | relationshipOrdered  = "NSMutableOrderedSet"
setTypeForRelation True Relationship{..}                        = "NSMutableSet"
setTypeForRelation False Relationship{..} | relationshipOrdered = "NSOrderedSet"
setTypeForRelation False Relationship{..}                       = "NSSet"

setForRelation :: Bool -> Relationship -> Doc ann
setForRelation mutable r = ptrTo (template (setTypeForRelation mutable r) [ ptrTo (nxAS <> dtext (relationshipDest r)) ])

orderedNonTransientAttributes :: Entity -> [Attribute]
orderedNonTransientAttributes e = sortBy (compare `on` attributeName) (filter (not . attributeTransient) (Map.elems $ entityAttributes e))

nsAttributeComparison :: AttributeType -> Doc ann -> Doc ann -> Doc ann
nsAttributeComparison AttributeString            a b = objcSend a ["isEqualToString", b]
nsAttributeComparison AttributeDate              a b = objcSend a ["isEqualToDate",   b]
nsAttributeComparison AttributeDuration          a b = objcSend a ["invalid",         b]
nsAttributeComparison AttributeDecimal           a b = objcSend a ["isEqualToNumber", b]
nsAttributeComparison (AttributeInteger _)       a b = objcSend a ["isEqualToNumber", b]
nsAttributeComparison AttributeBoolean           a b = objcSend a ["isEqualToNumber", b]
nsAttributeComparison (AttributeTransformable _) a b = objcSend a ["isEqualToData",   b]
nsAttributeComparison AttributeBinary            a b = objcSend a ["isEqualToData",   b]
nsAttributeComparison s _a _b = error $ "unsupported conversion " ++ show s

checkAttribute :: Entity -> Attribute -> [Doc ann]
checkAttribute _e Attribute{..} =
   [statement $ objcSend "self" ["get" <> (ucaseFirst . T.unpack $ attributeName)]]

decodeObjectId :: Doc ann -> ([Doc ann], Doc ann)
decodeObjectId = convertFromCocoa AttributeBinary

defaultTypeCtor :: AttributeType -> Doc ann
defaultTypeCtor AttributeString            = "String" /::/ call "string" []
defaultTypeCtor AttributeBinary            = "Blob" /::/ call "blob" []
defaultTypeCtor (AttributeTransformable _) = "Blob" /::/ call "blob" []
defaultTypeCtor _                          = mempty

defaultInitAttribute :: Attribute -> Doc ann
defaultInitAttribute a = call (baseName AllItems a) [defaultTypeCtor (attributeType a)]

ctorAttrs :: [Attribute] -> Doc ann -> Doc ann
ctorAttrs attrs doc =
   if null attrs
   then doc
   else doc /:/ commaList (map defaultInitAttribute attrs)

nxArb :: Con.Configuration -> Doc ann -> Doc ann
nxArb a = (nxA (joinNamespaces $ Con.configurationNamespaces a) /::/)

persistType :: Doc ann
persistType = schemaType/::/"Type"

nxATypeS :: Con.Configuration -> Doc ann
nxATypeS c = nxArb c persistType

nxAType :: Con.Configuration -> Doc ann -> Doc ann
nxAType c = (nxATypeS c /::/)

std :: Doc ann
std = "std"

mutableSetOf :: Scope -> Doc ann -> Doc ann
mutableSetOf s x = nxaNS s (template "MutableSet" [x])

setOf :: Scope -> Doc ann -> Doc ann
setOf s x = nxaNS s (template "Set" [x])

arrayOf :: Scope -> Doc ann -> Doc ann
arrayOf s x = nxaNS s (template "Array" [x])

optionalHeader, optionalNotPresent :: Doc ann
optionalHeader = mempty
optionalNotPresent  = nxaNS NoNamespace "nothing"

scopedFieldType :: AOR a => Con.Configuration -> Scope -> Visibility -> a -> Doc ann
scopedFieldType c s vis a = nxaNS s (fieldType (joinNamespaces $ Con.configurationNamespaces c) vis s a)

genericCopyright :: (Doc ann -> Doc ann) -> Doc ann
genericCopyright cchar = vcat $ map cchar
   [ ""
   , " Rekord Buddy - The future proof music collection tool made by DJs for DJs."
   , " Copyright (C) 2020-2021 Didier Malenfant (didier@rekordbuddy.org)"
   , ""
   , " This program is free software: you can redistribute it and/or modify"
   , " it under the terms of the GNU General Public License as published by"
   , " the Free Software Foundation, either version 3 of the License, or"
   , " (at your option) any later version."
   , ""
   , " This program is distributed in the hope that it will be useful,"
   , " but WITHOUT ANY WARRANTY; without even the implied warranty of"
   , " MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
   , " GNU General Public License for more details."
   , ""
   , " You should have received a copy of the GNU General Public License"
   , " along with this program.  If not, see https://www.gnu.org/licenses/."
   , ""
   ]

copyright :: Doc ann
copyright = genericCopyright lineComment

nxanamespace :: Doc ann -> Doc ann
nxanamespace a = "namespace NxA {" %% a %% statement "}"

returnInternal :: Doc ann -> Doc ann
returnInternal name = statement (returnD name)

assignInternal :: AOR a => a -> Doc ann -> Doc ann
assignInternal a value = statement (instanceName a /=/ value)

stringFormInclude :: EntityMap -> Entity -> Text
stringFormInclude ents e = entityName (ultimateParent ents e) `mappend` "/" `mappend` entityName e

data IncludeSubpath = PublicInclude | InternalInclude | ParentInternalInclude | ObjcInclude | ObjcMainInclude | ObjcCategoryInclude

generateInclude :: IncludeSubpath -> Con.Configuration -> Entity -> Doc ann
generateInclude ObjcMainInclude _ e =
  "#import" <+> dquotes (nxAS <> fieldName e <> ".h")
generateInclude ObjcInclude config e =
  includeSchema config (dtext (entityName (ultimateParent (Con.configurationEntities config) e)) <> "/ObjcWrapper/NxA" <> fieldName e <> "+CppCategory.h")
generateInclude InternalInclude config e =
  includeSchema config (dtext (entityName (ultimateParent (Con.configurationEntities config) e)) <> "/Internal/Persistent" <> fieldName e <> ".hpp")
generateInclude ParentInternalInclude config e =
   case entityParentName e of
      Just pe -> includeSchema config (dtext (entityName (ultimateParent (Con.configurationEntities config) e)) <> "/Persistent" <> dtext pe <> ".hpp")
      Nothing -> mempty
generateInclude PublicInclude config e =
  includeSchema config (dtext (entityName (ultimateParent (Con.configurationEntities config) e)) <> "/Persistent" <> fieldName e <> ".hpp")

nxaAssertTrue, nxaAssertNotNull, nxaAssertNotNil, nxaAssertNil :: Doc ann -> Doc ann
nxaAssertTrue    expr = statement (call "NXA_ASSERT_TRUE" [expr])
nxaAssertNotNull expr = statement (call "NXA_ASSERT_NOT_NULL" [expr])
nxaAssertNotNil  expr = statement (call "NxAAssertNotNil" [expr])
nxaAssertNil     expr = statement (call "NxAAssertNil" [expr])


attributeTypeToCocoaType :: AttributeType -> Doc ann
attributeTypeToCocoaType (AttributeInteger _)              = ptrTo "NSNumber"
attributeTypeToCocoaType AttributeString                   = ptrTo "NSString"
attributeTypeToCocoaType AttributeDate                     = ptrTo "NSDate"
attributeTypeToCocoaType AttributeDecimal                  = ptrTo "NSDecimalNumber"
attributeTypeToCocoaType AttributeBinary                   = ptrTo "NSData"
attributeTypeToCocoaType AttributeBoolean                  = ptrTo "NSNumber"
attributeTypeToCocoaType (AttributeTransformable (Just _)) = "id"
attributeTypeToCocoaType (AttributeTransformable _)        = "id"
attributeTypeToCocoaType AttributeDuration                 = ptrTo "id"
attributeTypeToCocoaType AttributeNone                     = ptrTo "NSNil"

-- | Given a Cocoa type, will produce a value compatible to the corresponding CPP type for core data interop
convertFromCocoa :: AttributeType -> Doc ann -> ([Doc ann], Doc ann)
convertFromCocoa AttributeString doc                   = ([], objcSend doc ["UTF8String"])
convertFromCocoa (AttributeInteger 8) doc              = ([], staticCast (nxaNS NoNamespace "integer8")  (objcSend doc ["charValue"]))
convertFromCocoa (AttributeInteger 16) doc             = ([], staticCast (nxaNS NoNamespace "integer16") (objcSend doc ["shortValue"]))
convertFromCocoa (AttributeInteger 32) doc             = ([], staticCast (nxaNS NoNamespace "integer32") (objcSend doc ["integerValue"]))
convertFromCocoa (AttributeInteger 64) doc             = ([], staticCast (nxaNS NoNamespace "integer64") (objcSend doc ["longLongValue"]))
convertFromCocoa AttributeBoolean doc                  = ([], objcSend doc ["boolValue"])
convertFromCocoa AttributeBinary doc                   = ([], objcSend doc ["blobValue"])
convertFromCocoa (AttributeTransformable Nothing) doc  = ([assign "NSData *data" (objcSend "NSKeyedArchiver" ["archivedDataWithRootObject", doc])], objcSend "data" ["blobValue"])
convertFromCocoa (AttributeTransformable (Just t)) doc = (
   [ statement ("NSValueTransformer * valueTransformer" /=/ objcSend "NSValueTransformer"
         [ "valueTransformerForName"
         , nsLiteralString (pretty t)])
   , statement ("NSData * data" /=/ objcSend "valueTransformer" ["transformedValue", doc])
   ], objcSend "data" ["blobValue"])
convertFromCocoa AttributeDate doc      = ([], objcSend doc ["timeIntervalSince1970"])
convertFromCocoa AttributeDecimal doc   = ([], call (nxA "decimal") [call ("std"/::/"string") [objcSend (objcSend doc ["stringValue"]) ["UTF8String"]]])
convertFromCocoa AttributeDuration _doc = error "Unknown Type"
convertFromCocoa AttributeNone _doc     = error "Unknown Type"

-- | Given a CPP type, will produce the corresponding cocoa type for core data interop
convertToCocoa :: AttributeType -> Doc ann -> ([Doc ann], Doc ann)
convertToCocoa AttributeString doc                  = ([], objcSend "NSString" ["stringWithUTF8String", doc /./ call "asUTF8" []])
convertToCocoa (AttributeInteger 8) doc             = ([], objcSend "NSNumber" ["numberWithChar", doc])
convertToCocoa (AttributeInteger 16) doc            = ([], objcSend "NSNumber" ["numberWithShort", doc])
convertToCocoa (AttributeInteger 32) doc            = ([], objcSend "NSNumber" ["numberWithInt", doc])
convertToCocoa (AttributeInteger 64) doc            = ([], objcSend "NSNumber" ["numberWithLongLong", doc])
convertToCocoa AttributeBoolean doc                 = ([], objcSend "NSNumber" ["numberWithBool", doc])
convertToCocoa AttributeBinary doc                  = ([], objcSend "NSData" ["dataWithBlob", doc])
convertToCocoa (AttributeTransformable Nothing) doc =
   ( [assign "NSData * data" (objcSend "NSData" ["dataWithBlob", doc])]
   , objcSend "NSKeyedUnarchiver" ["unarchiveObjectWithData", "data"])
convertToCocoa (AttributeTransformable (Just t)) doc = let (ctPrep, ctVal) = convertToCocoa AttributeBinary doc in
   (
   [ statement ("NSValueTransformer * valueTransformer" /=/ objcSend "NSValueTransformer"
         ["valueTransformerForName", nsLiteralString (pretty t)])
   ] ++ ctPrep ++
   [ statement ("NSData * data" /=/ ctVal)
   ], objcSend "valueTransformer" ["reverseTransformedValue", "data"])
convertToCocoa AttributeDate doc                     = ([], objcSend "NSDate" ["dateWithTimeIntervalSince1970", doc])
convertToCocoa AttributeDecimal doc                  = ([], objcSend "NSDecimalNumber" ["decimalNumberWithString", objcSend "NSString" ["stringWithUTF8String", call "NxA::Internal::toString" [doc] /./ "c_str()"]])
convertToCocoa AttributeDuration doc                 = error $ "Unknown Type for expr: " ++ show doc
convertToCocoa AttributeNone doc                     = error $ "Unknown Type for expr: " ++ show doc

destinationRelationship :: EntityMap -> Relationship -> Maybe Relationship
destinationRelationship es r = do
   e <- entityForName (Just $ relationshipDest r) es
   n <- relationshipInverseName r
   Map.lookup n (entityRelationships e)

relatedEntitiesForEntity :: Entity -> EntityMap -> [Entity]
relatedEntitiesForEntity e ents =
   nub $ catMaybes (entityForName (entityParentName e) ents : map (`entityOfRelationshipDestination` ents) (Map.elems (entityRelationships e)))

ucaseFirst :: String -> Doc ann
ucaseFirst []     = ""
ucaseFirst (a:as) = pretty (C.toUpper a : as)

-- categoryDecl :: Entity -> Attribute -> Doc ann
-- categoryDecl _e Attribute{..} =
--    let ucaseName = ucaseFirst . T.unpack $ attributeName
--    in (statement (objcMemberMethodDecl void ["set" <> ucaseName, parens (attributeTypeToCocoaType attributeType) <> "value"]))
--     %% (statement (objcMemberMethodDecl (attributeTypeToCocoaType attributeType) [ "get" <> ucaseName ]))

