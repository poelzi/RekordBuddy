{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns, DeriveDataTypeable #-}

import qualified Data.Map.Lazy as Map
import Prelude hiding (readFile)
import System.Console.CmdArgs
import Data.Maybe (fromMaybe, catMaybes)
import Data.List (nubBy, sortBy, intercalate)
import qualified Data.Text as T
import System.Directory (getCurrentDirectory, createDirectoryIfMissing)
import System.FilePath
import System.IO
import Data.Monoid ((<>))
import Control.Monad(when)
import RekordBuddy.CoreData.Parser
import RekordBuddy.Printer
import RekordBuddy.Types
import RekordBuddy.Configuration(Configuration(..))

mode :: Mode (CmdArgs DumpCD)
mode = cmdArgsMode $
   modes [generate] &= help "Tool to convert core data schema into various other things"
                    &= program "dpcd" &= summary "DumpCD v0.1"

generate :: DumpCD
generate = Generate
    { input                 = def &= help "The CoreData file to read (<destination>/<directoryName>/Persistence/Models/<namespace>V<outVersion>.xcdatamodeld/<namespace>.xcdatamodel/contents)" &= typ "FILE"
    , outVersion            = def &= help "Version number to use" &= typ "NUM"
    , isDefault             = def &= help "This version is the default version" &= typ "NUM"
    , minVersion            = def &= help "How far back can this version read successfully" &= typ "NUM"
    , destination           = def &= help "Directory where source will be generated (pwd)" &= typDir
    , namespace             = def &= help "The namespace where code will be injected" &= typ "STR"
    , innerNamespace        = def &= help "Inside the namespace; inject code into this an inner namespace" &= typ "STR"
    , directoryName         = def &= help "The directory where code will be placed (<namespace>)" &= typ "STR"
    , emitCoreDataMigration = def &= help "generate types that will migrate from core data"
    , emitCocoaWrappers     = def &= help "generate wrappers for cocoa types to ease migration"
    , nesting               = def &= help "How many spaces to indent generated source (4)"
    } &= help "Given a source schema, will produce a tree in destination dir"

data Format = Raw | RekordBuddy
             deriving (Data, Typeable, Show, Eq)

data Section = Public | Internal | Objc | All
             deriving (Data, Typeable, Show, Eq)

data Part = Header | Impl
             deriving (Data, Typeable, Show, Eq)

data DumpCD = Generate
   { destination :: Maybe FilePath
   , input :: Maybe FilePath
   , outVersion :: Int
   , isDefault :: Bool
   , minVersion :: Int
   , namespace :: String
   , innerNamespace :: Maybe String
   , directoryName :: Maybe String
   , emitCoreDataMigration :: Bool
   , emitCocoaWrappers :: Bool
   , nesting :: Maybe Int
   }
 deriving (Data, Typeable, Show, Eq)

outputToFilePath :: FilePath -> String -> String -> IO ()
outputToFilePath fp ext r =
   withFile (addExtension fp ext) WriteMode $ \h -> do
      hPutStr h r
      hPutStr h "\n"

collectionDirectory :: FilePath -> EntityMap -> Entity -> FilePath
collectionDirectory extroot ents ent = extroot </> T.unpack (entityName (ultimateParent ents ent))

objectFilename :: FilePath -> Entity -> FilePath
objectFilename cDir ent = cDir </> ("Persistent" ++ T.unpack (entityName ent))

-- | Perform the steps required to render an entity to the filesystem
generateEntity :: FilePath -> FilePath -> Bool -> Bool -> Configuration -> Entity -> IO ()
generateEntity objRoot incRoot migration wrappers config ent = do
   let objDir       = collectionDirectory objRoot (configurationEntities config) ent
   let incDir       = collectionDirectory incRoot (configurationEntities config) ent
   let upName       = T.unpack (entityName (ultimateParent (configurationEntities config) ent))
   let ename        = T.unpack $ entityName ent
   let (header, body, wrapper, wrapperHeader, entTest, migratorBody) = renderEntity config ent

   createDirectoryIfMissing True objDir
   createDirectoryIfMissing True incDir
   outputToFilePath (objectFilename incDir ent) "hpp" header
   outputToFilePath (objectFilename objDir ent) "cpp" body

   when wrappers (do
       let objcDir = objDir </> "ObjcWrapper"
       let inccDir = incDir </> "ObjcWrapper"
       let objTDir = objDir </> "ObjcTests" </> upName
       createDirectoryIfMissing True objcDir
       createDirectoryIfMissing True objTDir
       outputToFilePath (objTDir </> "NxA" ++ ename ++ "CollectionTests")  "mm" entTest
       outputToFilePath (inccDir </> "NxA" ++ ename ++ "+CppCategory")  "h" wrapperHeader
       outputToFilePath (objcDir </> "NxA" ++ ename ++ "+CppCategory")  "mm" wrapper)

   when migration (do
       let migTDir = objDir </> "CoreDataMigration" </> upName
       createDirectoryIfMissing True migTDir
       outputToFilePath (migTDir </> ename ++ "Migration")  "cpp" migratorBody)

   return ()

sameRelationship :: Relationship -> Relationship -> Bool
sameRelationship ra rb = ra == rb

compareRelation :: Relationship -> Relationship -> Ordering
compareRelation = compare

uniqueManyToManyRelationships :: EntityMap -> [ManyToMany]
uniqueManyToManyRelationships ents =
   let prims = nubBy sameRelationshipOrInverseSame (sortBy compareRelation (concatMap manyToManyForEnt entitiesAsList))
   in map (relationshipAndInverse ents) prims
 where
   entitiesAsList = Map.elems ents
   sameRelationshipOrInverseSame ra rb =
      sameRelationship ra rb || fmap (sameRelationship ra) (inverseRelationship rb ents) == Just True
   isManyToMany r = relationshipToMany r && fmap relationshipToMany (inverseRelationship r ents) == Just True
   manyToManyForEnt ent = filter isManyToMany (Map.elems $ entityRelationships ent)

main :: IO ()
main = do
   pwd <- getCurrentDirectory
   options <- cmdArgsRun mode
   case options of
      Generate {..} -> do
         let outDir = fromMaybe pwd destination
         let defaultDirName = fromMaybe (intercalate "/" (catMaybes [Just namespace, innerNamespace])) directoryName
         let colRoot = outDir </> defaultDirName </> "Persistence"
         let objRoot      = colRoot </> "Objects" </> ("V" ++ show outVersion)
         let incRoot      = colRoot </> "Include" </> concat (catMaybes [Just namespace, innerNamespace, Just "Persistence"]) </> ("V" ++ show outVersion)
         let defaultModel = colRoot </> "Models" </> namespace <> ".xcdatamodeld" </> namespace <> "V" <> show outVersion <> ".xcdatamodel" </> "contents"
         ments <- readSchema (fromMaybe defaultModel input)
         createDirectoryIfMissing True objRoot
         createDirectoryIfMissing True incRoot
         case ments of
            Left err   -> fail err
            Right ents -> do
               let config = Configuration { configurationRoot = T.pack defaultDirName
                                          , configurationNamespaces = catMaybes [Just (T.pack namespace), (fmap T.pack innerNamespace)]
                                          , configurationVersion = outVersion
                                          , nesting = fromMaybe 4 nesting
                                          , configurationManyToMany = uniqueManyToManyRelationships ents
                                          , configurationEntities = ents
                                          , minVersion = minVersion
                                          , configurationCocoaWrappers = emitCocoaWrappers
                                          , defaultVersion = isDefault
                                          }
               outputToFilePath (incRoot </> "AllPersistentObjects") "ipp" (renderRegistrationHeader config)
               outputToFilePath (objRoot </> (("V" ++ show outVersion) ++ "Schema")) "cpp" (renderVersionBody config)
               outputToFilePath (incRoot </> "Version") "ipp" (renderVersionFile config)
               outputToFilePath (colRoot </> "CMakeLists") "txt" (renderCMakeLists config)
               outputToFilePath (incRoot </> "MetaMacroAllTypes") "ipp" (renderMetaMacroHeader config)
               when emitCocoaWrappers (outputToFilePath (incRoot </> "AllObjectiveCWrapperTypes") "h" (renderAllObjectiveCWrapperHeaders config))
               mapM_ (generateEntity objRoot incRoot emitCoreDataMigration emitCocoaWrappers config) ents


