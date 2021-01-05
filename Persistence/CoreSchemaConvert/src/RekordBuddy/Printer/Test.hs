{-# LANGUAGE OverloadedStrings, DisambiguateRecordFields, RecordWildCards #-}
module RekordBuddy.Printer.Test(entityTestDoc) where

import           Data.Text.Prettyprint.Doc
import qualified Data.Map.Lazy as Map
import           Data.List (sortBy)
import           Data.Function (on)
import           RekordBuddy.Types
import qualified RekordBuddy.Configuration as Con
import           RekordBuddy.Helpers
import           RekordBuddy.Printer.NxA

entityTestDoc :: Con.Configuration -> Entity -> Doc ann
entityTestDoc config e =
   vsep (copyright : prologue : [entityTest config e])
 where
   prologue = vcat
      [ includeRoot config ((dtext $ head (Con.configurationNamespaces config)) <>"Schema.hpp")
      , objcImport "Persistence/Internal/NxAPersistentObject.h"
      , objcImport "NxATestCase.h"
      ]

testName :: Doc ann -> Doc ann -> Doc ann -> Doc ann
testName action given result = cat (punctuate "_" ["testp", action, given, result])

testCase :: Int -> (Doc ann, [Doc ann]) -> (Doc ann, [Doc ann]) -> (Doc ann, [Doc ann]) -> Doc ann
testCase nnesting (a, as) (b, bs) (c, cs) =
   objcMemberMethodDef nnesting "void" [testName b a c]
      [ vsep
         [ vcat ((pragmaMark "Given" <+> a) : as)
         , vcat ((pragmaMark "When" <+> b) : bs)
         , vcat ((pragmaMark "Then" <+> c) : cs)
         ]
      ]

entityTest :: Con.Configuration -> Entity -> Doc ann
entityTest config e = vsepCat
   [  [ pragmaMark "Private Interface"
      , objcInterface (ename <> "CollectionTests" /:/ "NxATestCase") [ ]
      ]
   ,  [ pragmaMark "Implementation"
      , objcImplementation (ename <> "CollectionTests")
         ( pragmaMark "Test methods"
         : attributeTest config attrs (candidates e) e
         : map (relationshipTest config e) (relns e)
         )
      ]
   ]
 where
   ename = nxAS <> fieldName e
   attrs = filter testableAttribute (orderedNonTransientAttributes e)
   relns ee = Map.elems (entityRelationships ee) ++ maybe [] relns (parentEntity (Con.configurationEntities config) ee)
   candidates c =
      if entityIsAbstract c
      then sortBy (compare `on` entityName) $ filter (not . entityIsAbstract) (allDescendents c)
      else [c]
   allDescendents c = let hereDescendents = filter (\x -> entityParentName x == Just (entityName c)) (Map.elems (Con.configurationEntities config))
                       in concatMap allDescendents hereDescendents ++ hereDescendents

cascadeTest :: Con.Configuration -> Relationship -> Entity -> Doc ann
cascadeTest config r e |
      relationshipRule r == Cascade &&
      relationshipToMany r &&
      fmap relationshipToMany (inverseRelationship r (Con.configurationEntities config)) /= Just True =
   testCase (Con.nesting config)
      ( fieldName e <> titleCaseName AllItems r,
         [ assign ("NxAPersistentContext" <+> deref "context") (objcSend "NxAPersistentContext" ["contextInMemory"])
         , assign "using Type" (nxATypeS config)
         , assign "using OID" (nxArb config "ObjectID")
         , assign ("NxAPersistentObject" <+> deref "fetch1") "nil"
         , assign (ename e <+> deref "object1") (newObject config e)
         , assign (ename e <+> deref "object2") (newObject config e)
         , callS "XCTAssertEqual" ["object1"/./ fieldName r /./ "count", "0"]
         ] ++
         inverseCandidate (Con.configurationEntities config) r (\ae ->
            [ assign (ename ae <+> deref "po1") (newObject config ae)
            , statement $ objcSend "object1" [wrapperAdderNameP r, "po1"]
            ])
      )
      ( "DeleteObject"
      , [ statement (objcSend "context" ["deleteObject", "object1"])
        , statement (objcSend "context" ["deleteObject", "object2"])
        , statement (objcSend "context" ["deleteObject", "object2"]) <+> pragmaMark "test double-delete"
        ] ++
         inverseCandidate (Con.configurationEntities config) r
            (const
               [ assign "fetch1" (objcSend "context" ["objectWithID", "po1"/./"objectID"])
               ]
            )
      )
      ( "Cascades"
      , [ callS "XCTAssertTrue" [objcSend "object1" ["isDeleted"]]
        , callS "XCTAssertTrue" [objcSend "object2" ["isDeleted"]]
        , callS "XCTAssertTrue" [objcSend "po1" ["isDeleted"]]
        , callS "XCTAssertNil" ["fetch1"]
        ]
      )
 where
   ename ee = nxAS <> fieldName ee
cascadeTest _nesting _r _e = mempty

relationshipTest :: Con.Configuration -> Entity -> Relationship -> Doc ann
relationshipTest config e r |
      not (relationshipToMany r) &&
      fmap relationshipToMany (inverseRelationship r (Con.configurationEntities config)) /= Just True =
   if entityIsAbstract e || fmap entityIsAbstract (entityForName (Just $ relationshipDest r) (Con.configurationEntities config)) == Just True then mempty
   else vsep [
      testCase (Con.nesting config)
         (fieldName e <> titleCaseName AllItems r,
            [ assign ("NxAPersistentContext" <+> deref "context") (objcSend "NxAPersistentContext" ["contextInMemory"])
            , assign (ename <+> deref "object1") (newObject config e)
            , assign (rname <+> deref (fieldName r <> "1")) (newObject config r)
            , if fieldOptional r then assign (rname <+> deref (fieldName r <> "2")) (newObject config r) else mempty
            , callS "XCTAssertNil" ["object1"/./fieldName r]
            ] ++ [assign (ename <+> deref "object2") (newObject config e) | fieldOptional r]
         )
         ("ModifyOneToOne",
            assign ("object1" /./ fieldName r) (fieldName r <> "1") :
            (if fieldOptional r
            then
               [ assign ("object2" /./ fieldName r) (fieldName r <> "2")
               , assign ("object2" /./ fieldName r) "nil"
               ]
            else []
            )
         )
         ("InverseChanged",
            callS "XCTAssertEqual"
               [ fieldName r <> "1" /./ maybe "__error__" fieldName (inverseRelationship r (Con.configurationEntities config))
               , "object1"
               ]
            : [callS "XCTAssertNil" ["object2" /./ fieldName r] | fieldOptional r]
         )
         , cascadeTest config r e
   ]
 where
   ename = nxAS <> fieldName e
   rname = nxAS <> dtext (relationshipDest r)

relationshipTest config e r |
      relationshipToMany r &&
      fmap relationshipToMany (inverseRelationship r (Con.configurationEntities config)) == Just True =
   if entityIsAbstract e then mempty
   else vsep [
      testCase (Con.nesting config)
         (fieldName e <> titleCaseName AllItems r,
            [ assign ("NxAPersistentContext" <+> deref "context") (objcSend "NxAPersistentContext" ["contextInMemory"])
            , assign "using Type" (nxATypeS config)
            , assign "using OID" (nxArb config "ObjectID")
            , assign (ename e <+> deref "object1") (newObject config e)
            , callS "XCTAssertEqual" ["object1"/./ fieldName r /./ "count", "0"]
            ] ++
            inverseCandidate (Con.configurationEntities config) r (\ae -> replicate 6 (statement $ objcSend "object1" [wrapperAdderNameP r, newObject config ae]))
         )
         ("ReorderManyToMany",
            inverseCandidate (Con.configurationEntities config) r (\ae ->
               [ statement $ "OID"
                  <+> name <> braceList [number, fieldType (joinNamespaces (Con.configurationNamespaces config)) Public SchemaScoped ae, nxA (joinNamespaces (Con.configurationNamespaces config)/::/schemaType/::/"schemaVersion()")] | (name,number) <- [("two","2"), ("three","3"), ("four","4")]]
               ++
               [ callS ("object1"/./"cppObject"/->/cppOrderNameP ItemsByIndex r) [braceList [ "two", "four" ], "3"]
               ]
            )
         )
         ("OrderChanged",
            autoInit "&& col" ("object1"/./"cppObject"/->/ call (cppGetterNameP AllItems r) []) :
               [ vcat [ autoInit ("a" <> index) ("col" <> brackets index)
                      , callS "XCTAssertEqual" [("a" <> index) /->/ call "objectID" [], name]
                      ] | (name, index) <- [("three", "0"), ("two", "1"), ("four", "2")]]
         )
      , cascadeTest config r e
      ,testCase (Con.nesting config)
         (fieldName e <> titleCaseName AllItems r,
            [ assign ("NxAPersistentContext" <+> deref "context") (objcSend "NxAPersistentContext" ["contextInMemory"])
            , assign (ename e <+> deref "object1") (newObject config e)
            , callS "XCTAssertEqual" ["object1"/./ fieldName r /./ "count", "0"]
            ]
         )
         ("ModifyManyToMany",
            inverseCandidate (Con.configurationEntities config) r (\ae ->
               [ statement $ objcSend "object1" [wrapperAdderNameP r, newObject config ae]
               , assign (ename ae <+> deref "cand1") (newObject config ae)
               , statement $ objcSend "object1" [wrapperAdderNameP r, "cand1"]
               , statement $ objcSend "object1" [wrapperRemoverNameP r, "cand1"]
               ])
         )
         ("InverseChanged",
            inverseCandidate (Con.configurationEntities config) r (\ae ->
              [callS "XCTAssertEqual" [objcSend "object1" [fieldName r <> "Of", nxAType config (fieldName ae)]/./"count", "1"]]
            )
         )
   ]
 where
   ename ee = nxAS <> fieldName ee

relationshipTest config e r |
      fmap relationshipToMany (inverseRelationship r (Con.configurationEntities config)) /= Just True =
   if entityIsAbstract e then mempty
   else vsep [
      testCase (Con.nesting config)
         (fieldName e <> titleCaseName AllItems r,
            [ assign ("NxAPersistentContext" <+> deref "context") (objcSend "NxAPersistentContext" ["contextInMemory"])
            , assign (ename <+> deref "object1") (newObject config e)
            , assign (rname <+> deref (fieldName r <> "1")) (newObject config r)
            ] ++ (
            if fieldOptional r
            then [
                   assign (ename <+> deref "object2") (newObject config e)
                 , assign (rname <+> deref (fieldName r <> "2")) (newObject config r)
                 ]
            else [])
         )
         ("ModifyManyToOne",
            statement (objcSend  "object1" [wrapperAdderNameP r, fieldName r <> "1" ])
            : (if fieldOptional r
               then [ statement $ objcSend  "object2" [wrapperAdderNameP r, fieldName r <> "2"]
                    , statement $ objcSend  "object2" [wrapperRemoverNameP r, fieldName r <> "2"]
                    ]
               else []
              )
         )
         ("InverseChanged",
            [ callS "XCTAssertEqual"
                [ fieldName r <> "1" /./ maybe "__error__" fieldName (inverseRelationship r (Con.configurationEntities config))
                , "object1"
                ]
            , callS "XCTAssertEqual" [objcSend "object1" [fieldName r <>"Of", nxAType config (dtext (relationshipDest r))] /./ "count", "1"]
            ] ++ [ callS "XCTAssertEqual" ["object2" /./ fieldName r /./ "count", "0"] | fieldOptional r]
         )
      , cascadeTest config r e
      ]
 where
   ename = nxAS <> fieldName e
   rname = nxAS <> dtext (relationshipDest r)


relationshipTest config e r |
      fmap relationshipToMany (inverseRelationship r (Con.configurationEntities config)) == Just True =
   if entityIsAbstract e || fmap entityIsAbstract (entityForName (Just $ relationshipDest r) (Con.configurationEntities config)) == Just True then mempty
   else vsep [
      testCase (Con.nesting config)
         (fieldName e <> titleCaseName AllItems r,
            [ assign ("NxAPersistentContext" <+> deref "context") (objcSend "NxAPersistentContext" ["contextInMemory"])
            , assign (ename <+> deref "object1") (newObject config e)
            , assign (rname <+> deref (fieldName r <> "1")) (newObject config r)
            , callS "XCTAssertNil" ["object1"/./fieldName r]
            ] ++ (
            if fieldOptional r
            then [ assign (rname <+> deref (fieldName r <> "2")) (newObject config r)
                 , assign (ename <+> deref "object2") (newObject config e)
                 ]
            else [])
         )
         ("ModifyOneToMany",
            assign ("object1" /./ fieldName r) (fieldName r <> "1")
            : (if fieldOptional r
               then
                  [ assign ("object2" /./ fieldName r) (fieldName r <> "2")
                  , assign ("object2" /./ fieldName r) "nil"
                  ]
               else []
               )
         )
         ("InverseChanged",
             callS "XCTAssertTrue"
               [objcSend (fieldName r <> "1" /./ maybe "__error__" fieldName (inverseRelationship r (Con.configurationEntities config))) ["containsObject", "object1"]]
               : [callS "XCTAssertNil" ["object2" /./ fieldName r] | fieldOptional r]
         )
      , cascadeTest config r e
      ]
 where
   ename = nxAS <> fieldName e
   rname = nxAS <> dtext (relationshipDest r)

attributeTest :: Con.Configuration -> [Attribute] -> [Entity] -> Entity -> Doc ann
attributeTest config attrs concretes e =
   if null attrs || null concretes then mempty
   else
      testCase (Con.nesting config)
         (fieldName e,
            [ assign ("NxAPersistentContext" <+> deref "context") (objcSend "NxAPersistentContext" ["contextInMemory"])
            , assign (ename <+> deref "object1") (newObject config (head concretes))
            , callS "XCTAssertEqual" [objcSend ename ["objectType"], nxAType config (fieldName (head concretes))]
            ]
            ++ [assign (ename <+> deref "object2") (newObject config (head concretes)) | any attributeOptional attrs]
            ++ map attributeGiven attrs)
         ("UpdateAttributes", map attributeWhen attrs)
         ("IsChanged", map attributeThen attrs)
 where
   ename = nxAS <> fieldName (head concretes)
   attributeGiven a = vcat (
       assign (attributeTypeToCocoaType (attributeType a) <+> dtext (attributeName a))
               (instanceForType (attributeType a))
      : (if attributeOptional a
         then [ callS "XCTAssertNil" ["object1" /./ dtext (attributeName a)], callS "XCTAssertNil" ["object2" /./ dtext (attributeName a)]
              ]
         else [ callS "XCTAssertTrue" [
                  objcSend ("object1" /./ dtext (attributeName a)) ["isEqual", defaultInstanceForType (attributeType a)]]
              ]))

   attributeWhen a = vcat
      ( assign ("object1" /./ dtext (attributeName a)) (dtext (attributeName a))
      : (if attributeOptional a
         then [assign ("object2" /./ dtext (attributeName a)) (dtext (attributeName a)), assign ("object2" /./ dtext (attributeName a)) "nil"]
         else []))

   attributeThen a = vcat (
         callS "XCTAssertTrue" [objcSend ("object1" /./ dtext (attributeName a)) ["isEqual", dtext (attributeName a)]]
      : [callS "XCTAssertNil" ["object2" /./ dtext (attributeName a)] | attributeOptional a])

inverseCandidate :: EntityMap -> Relationship -> (Entity -> [Doc ann]) -> [Doc ann]
inverseCandidate ees rr f =
         case entityForName (Just $ relationshipDest rr) ees of
            Nothing -> [pragmaMark "No inverse for type (1)"]
            Just en -> case maybeChildType ees en of
               Nothing -> [pragmaMark "No inverse for type (2)"]
               Just ae -> f ae

testableAttribute :: Attribute -> Bool
testableAttribute Attribute{attributeType = AttributeInteger _} = True
testableAttribute Attribute{attributeType = AttributeString}    = True
testableAttribute Attribute{attributeType = AttributeDate}      = True
testableAttribute Attribute{attributeType = AttributeDecimal}   = True
testableAttribute Attribute{attributeType = AttributeDuration}  = True
testableAttribute Attribute{attributeType = AttributeBinary}    = True
testableAttribute Attribute{attributeType = AttributeBoolean}   = True
testableAttribute _                                             = False

defaultInstanceForType :: AttributeType -> Doc ann
defaultInstanceForType (AttributeInteger _) = "@(0)"
defaultInstanceForType AttributeString      = nsLiteralString ""
defaultInstanceForType AttributeDate        = objcSend "NSDate" ["dateWithTimeIntervalSince1970", "0"]
defaultInstanceForType AttributeDecimal     = objcSend "NSDecimalNumber" ["decimalNumberWithString", nsLiteralString "0"]
defaultInstanceForType AttributeBinary      = objcSend "NSData" ["data"]
defaultInstanceForType AttributeBoolean     = objcSend "NSNumber" ["numberWithBool", "false"]
defaultInstanceForType _                    = pragmaMark "Not supported yet"

instanceForType :: AttributeType -> Doc ann
instanceForType (AttributeInteger _) = "@(42)"
instanceForType AttributeString      = nsLiteralString "SomeString"
instanceForType AttributeDate        = objcSend "NSDate" ["dateWithTimeIntervalSince1970", "123456"]
instanceForType AttributeDecimal     = objcSend "NSDecimalNumber" ["decimalNumberWithString", nsLiteralString "1.1"]
instanceForType AttributeBinary      = objcSend "NSData" ["data"]
instanceForType AttributeBoolean     = objcSend "NSNumber" ["numberWithBool", "true"]
instanceForType _                    = pragmaMark "Not supported yet"

