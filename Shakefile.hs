#!/usr/bin/env stack
{- stack
  script
  --resolver lts-13.13
  --ghc-options -ivegetables/tools
  --package containers
  --package directory
  --package extra
  --package filepath
  --package MissingH
  --package process
  --package shake
  --package split
-}
import           Control.Applicative            ( (<|>) )
import           Control.Exception              ( catch
                                                , throwIO
                                                )
import           Control.Monad                  ( filterM
                                                , foldM
                                                , liftM
                                                )
import           Data.Char                      ( isAsciiLower
                                                , isDigit
                                                , toLower
                                                )
import           Data.List                      ( sort )
import           Data.List.Split                ( splitOn )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Set                      as Set
import           Data.String.Utils              ( endswith )
import           Development.Shake              ( Action
                                                , Change(ChangeModtimeAndDigest)
                                                , CmdOption(Cwd)
                                                , Exit(Exit)
                                                , FilePattern
                                                , Resource
                                                , Rules
                                                , Stderr(Stderr)
                                                , action
                                                , actionOnException
                                                , alwaysRerun
                                                , cmd
                                                , command
                                                , command_
                                                , copyFileChanged
                                                , getDirectoryFilesIO
                                                , getShakeOptions
                                                , liftIO
                                                , need
                                                , newResource
                                                , phony
                                                , progressSimple
                                                , removeFilesAfter
                                                , shakeArgs
                                                , shakeChange
                                                , shakeColor
                                                , shakeFiles
                                                , shakeOptions
                                                , shakeProgress
                                                , shakeThreads
                                                , want
                                                , withResource
                                                , (?==)
                                                , (?>)
                                                , (&?>)
                                                , (<//>)
                                                )
import           Development.Shake.FilePath     ( dropDirectory1
                                                , dropExtension
                                                , exe
                                                , takeDirectory
                                                , takeDirectory1
                                                , takeExtension
                                                , takeFileName
                                                , (-<.>)
                                                , (<.>)
                                                , (</>)
                                                )
import           Juicer                         ( makeDriver )
import           System.Directory               ( doesFileExist
                                                , getCurrentDirectory
                                                , removeFile
                                                )
import           System.Exit                    ( ExitCode
                                                    ( ExitFailure
                                                    , ExitSuccess
                                                    )
                                                )
import           System.IO.Error                ( isDoesNotExistError )
import           System.Process                 ( readProcessWithExitCode )
import           Text.ParserCombinators.ReadP   ( ReadP
                                                , char
                                                , eof
                                                , many
                                                , many1
                                                , option
                                                , pfail
                                                , readP_to_S
                                                , satisfy
                                                , skipSpaces
                                                , string
                                                )


-- All the extra type declarations we need
type ModuleName = String
type ProgramName = String
type ModuleNames = Set.Set ModuleName
type SpecialCommands = Map.Map FilePath String

data SourceFile =
    SourceFile {
          getFileName           :: FilePath
        , getModulesUsed :: ModuleNames
        , getModulesContained   :: ModuleNames
    } deriving (Eq, Ord, Show)

data Module = Module {
      getModuleName   :: ModuleName
    , getModuleSource :: SourceFile
} deriving (Eq, Ord, Show)

data Program = Program {
      getProgramName   :: ProgramName
    , getProgramSource :: SourceFile
} deriving (Eq, Ord, Show)

type SourceFiles = Set.Set SourceFile
type Modules = Map.Map ModuleName Module
type Programs = Set.Set Program

data LineContents =
      ModuleUsed ModuleName
    | ModuleDefined ModuleName
    | ProgramDefined ProgramName
    | Other deriving (Show)

data ModuleSearchTree =
    Node {
      getNodeDirectory  :: FilePath
    , getNodeModules    :: Modules
    , getRemainingNodes :: Set.Set ModuleSearchTree
} deriving (Eq, Ord, Show)

-- A handful of constants that are just basic configuration

-- compiler and flags
compiler = "gfortran"
sourceExts = [".f90", ".f", ".F", ".F90", ".f95", ".f03"]
compilerFlags = ["-g", "-Wall", "-Wextra", "-Werror", "-pedantic"]
linkFlags = compilerFlags ++ ["-lgfortran"]

-- directories
sourceDirs =
    [ "src"
    , "erloff" </> "src"
    , "iso_varying_string" </> "src"
    , "strff" </> "src"
    ]
shakeDir = "_shake"
buildDir = "build"
testDir = "tests"
testBuildDir = "tests_build"

-- Special files
testDriverName = "vegetable_driver"
testDriverSourceFile = testBuildDir </> testDriverName <.> "f90"

-- The main routine that executes the build system
main :: IO ()
main = do
    sourceFiles                  <- getDirectoriesFiles sourceDirs sourceExts
    (sources, modules, programs) <- scanSourceFiles Map.empty sourceFiles
    let projectSearchTree = Node buildDir modules Set.empty

    testSourceFiles <- getDirectoriesFiles [testDir] sourceExts
    let testCollectionFiles = filter (endswith "_test.f90") testSourceFiles
    let testCollectionModules = Set.fromList
            $ map (takeFileName . dropExtension) testCollectionFiles
    let testDriverSource = SourceFile
            testDriverSourceFile
            ("vegetables_m" `Set.insert` testCollectionModules)
            Set.empty
    let testDriverProgram = Program testDriverName testDriverSource
    (testSources', testModules, testPrograms') <- scanSourceFiles
        modules
        (("vegetables" </> "src" </> "Vegetables_m.f90") : testSourceFiles)
    let testSources  = testDriverSource `Set.insert` testSources'
    let testPrograms = testDriverProgram `Set.insert` testPrograms'
    let testSearchTree =
            Node testBuildDir testModules (Set.singleton projectSearchTree)

    let (projectFilesMap, projectDependsMap, projectCommandsMap) =
            makeSourceRuleMaps compilerFlags
                               projectSearchTree
                               (Set.toList sources)
    let (testFilesMap, testDependsMap, testCommandsMap) = makeSourceRuleMaps
            compilerFlags
            testSearchTree
            (Set.toList testSources)
    let (filesMap, dependsMap, commandsMap) =
            ( projectFilesMap `Map.union` testFilesMap
            , projectDependsMap `Map.union` testDependsMap
            , projectCommandsMap `Map.union` testCommandsMap
            )
    let (projectExecutableSet, projectLinkMap) =
            makeExecutableMaps projectSearchTree (Set.toList programs)
    let (testExecutableSet, testLinkMap) =
            makeExecutableMaps testSearchTree (Set.toList testPrograms)
    let (allExecutableSet, allLinkMap) =
            ( projectExecutableSet `Set.union` testExecutableSet
            , projectLinkMap `Map.union` testLinkMap
            )

    shakeArgs shakeOptions { shakeFiles    = shakeDir
                           , shakeChange   = ChangeModtimeAndDigest
                           , shakeColor    = True
                           , shakeThreads  = 0
                           , shakeProgress = progressSimple
                           }
        $ do
              (`Map.lookup` filesMap) &?> \(obj : _) -> do
                  need =<< liftIO
                      (fromMaybe undefined (Map.lookup obj dependsMap))
                  fromMaybe undefined (Map.lookup obj commandsMap)

              (`Set.member` allExecutableSet) ?> \exe -> do
                  linkTimeDepends <- liftIO
                      (fromMaybe undefined (Map.lookup exe allLinkMap))
                  need linkTimeDepends
                  cmd compiler linkTimeDepends ["-o", exe] linkFlags

              (== testDriverSourceFile) ?> \driver -> do
                  need testCollectionFiles
                  createDriver driver testCollectionFiles

              want ["tests"]
              phony "project"
                  $ need
                        [ buildDir </> getProgramName prog <.> exe
                        | prog <- Set.toList programs
                        ]
              phony "tests" $ do
                  let progs =
                          [ testBuildDir </> getProgramName prog <.> exe
                          | prog <- Set.toList testPrograms
                          ]
                  need progs
                  mapM_ (cmd :: FilePath -> Action ()) progs
              phony "all" $ need ["project", "tests"]
              phony "cleanProject" $ removeFilesAfter buildDir ["//"]
              phony "cleanTest" $ removeFilesAfter testBuildDir ["//"]
              phony "clean" $ need ["cleanProject", "cleanTest"]
              phony "cleanShake" $ removeFilesAfter shakeDir ["//"]
              phony "cleanAll" $ need ["clean", "cleanShake"]

-- A little wrapper around getDirectoryFiles so we can get files from multiple directories
getDirectoriesFiles :: [FilePath] -> [FilePattern] -> IO [FilePath]
getDirectoriesFiles dirs exts = getDirectoryFilesIO "" newPatterns
  where
    newPatterns = concatMap appendExts dirs
    appendExts dir = map ((dir <//> "*") ++) exts

-- The routines we need to parse all the source files
scanSourceFiles :: Modules -> [FilePath] -> IO (SourceFiles, Modules, Programs)
scanSourceFiles previousModules = foldl
    addNextContents
    (return (Set.empty, Map.empty, Set.empty))
  where
    addNextContents previousContents file = do
        (sources, modules, programs) <- previousContents
        newContents <- scanSourceFile (previousModules `Map.union` modules) file
        case newContents of
            Left (source, program) ->
                return
                    ( source `Set.insert` sources
                    , modules
                    , program `Set.insert` programs
                    )
            Right (source, newModules) ->
                return
                    ( source `Set.insert` sources
                    , newModules `Map.union` modules
                    , programs
                    )

scanSourceFile
    :: Modules
    -> FilePath
    -> IO (Either (SourceFile, Program) (SourceFile, Modules))
scanSourceFile previousModules file = do
    fileLines <- readFileLinesIO file
    let eitherContents = foldl addLineContents
                               (Right (Set.empty, Set.empty, Nothing))
                               fileLines
    case eitherContents of
        Right contents -> case contents of
            (modulesContained, modulesUsed, Nothing) ->
                return
                    $ Right
                          (buildOutputWithModules file
                                                  modulesUsed
                                                  modulesContained
                          )
            (modulesContained, modulesUsed, Just programName) ->
                return
                    $ Left
                          (buildOutputWithProgram file
                                                  modulesUsed
                                                  modulesContained
                                                  programName
                          )
        Left err -> fail $ "*** Error in file " ++ file ++ ": " ++ err
  where
    addLineContents contents line = case contents of
        Right (previousModulesContained, previousModulesUsed, maybeProgram) ->
            case parseFortranLine line of
                ModuleUsed moduleName -> Right
                    ( previousModulesContained
                    , moduleName `Set.insert` previousModulesUsed
                    , maybeProgram
                    )
                ModuleDefined moduleName ->
                    case Map.lookup moduleName previousModules of
                        Just module' ->
                            Left
                                $  "module "
                                ++ moduleName
                                ++ " was already defined in "
                                ++ getFileName (getModuleSource module')
                        Nothing ->
                            if moduleName `Set.member` previousModulesContained
                                then
                                    Left
                                    $  "module "
                                    ++ moduleName
                                    ++ " defined twice"
                                else Right
                                    ( moduleName
                                        `Set.insert` previousModulesContained
                                    , previousModulesUsed
                                    , maybeProgram
                                    )
                ProgramDefined programName -> case maybeProgram of
                    Just programName' ->
                        Left
                            $  "multiple programs defined: "
                            ++ programName'
                            ++ " and "
                            ++ programName
                    Nothing ->
                        Right
                            ( previousModulesContained
                            , previousModulesUsed
                            , Just programName
                            )
                Other ->
                    Right
                        ( previousModulesContained
                        , previousModulesUsed
                        , maybeProgram
                        )
        Left err -> Left err

readFileLinesIO :: FilePath -> IO [String]
readFileLinesIO file = do
    contents <- readFile file
    return $ lines contents

buildOutputWithModules
    :: FilePath -> ModuleNames -> ModuleNames -> (SourceFile, Modules)
buildOutputWithModules file modulesUsed modulesContained =
    let source = SourceFile file
                            (modulesUsed `Set.difference` modulesContained)
                            modulesContained
        modules = foldl (addModuleWithSource source) Map.empty modulesContained
    in  (source, modules)
  where
    addModuleWithSource source previousModules moduleName =
        Map.insert moduleName (Module moduleName source) previousModules

buildOutputWithProgram
    :: FilePath
    -> ModuleNames
    -> ModuleNames
    -> ProgramName
    -> (SourceFile, Program)
buildOutputWithProgram file modulesUsed modulesContained programName =
    let source = SourceFile file
                            (modulesUsed `Set.difference` modulesContained)
                            modulesContained
        program = Program programName source
    in  (source, program)

parseFortranLine :: String -> LineContents
parseFortranLine line =
    let line'  = map toLower line
        result = readP_to_S doFortranLineParse line'
    in  getResult result
  where
    getResult (_ : (contents, _) : _) = contents
    getResult [(contents, _)        ] = contents
    getResult []                      = Other

doFortranLineParse :: ReadP LineContents
doFortranLineParse = option Other fortranUsefulContents

fortranUsefulContents :: ReadP LineContents
fortranUsefulContents =
    moduleDeclaration <|> programDeclaration <|> useStatement

moduleDeclaration :: ReadP LineContents
moduleDeclaration = do
    skipSpaces
    _ <- string "module"
    skipAtLeastOneWhiteSpace
    modName <- validIdentifier
    skipSpaceOrEnd
    if modName == "procedure" then pfail else return $ ModuleDefined modName

programDeclaration :: ReadP LineContents
programDeclaration = do
    skipSpaces
    _ <- string "program"
    skipAtLeastOneWhiteSpace
    progName <- validIdentifier
    skipSpaceOrEnd
    return $ ProgramDefined progName

useStatement :: ReadP LineContents
useStatement = do
    skipSpaces
    _ <- string "use"
    skipAtLeastOneWhiteSpace
    modName <- validIdentifier
    skipSpaceCommaOrEnd
    return $ ModuleUsed modName

skipAtLeastOneWhiteSpace :: ReadP ()
skipAtLeastOneWhiteSpace = do
    _ <- many1 whiteSpace
    return ()

skipSpaceOrEnd :: ReadP ()
skipSpaceOrEnd = eof <|> skipAtLeastOneWhiteSpace

skipSpaceCommaOrEnd :: ReadP ()
skipSpaceCommaOrEnd = eof <|> skipComma <|> skipAtLeastOneWhiteSpace

skipComma :: ReadP ()
skipComma = do
    _ <- char ','
    return ()

whiteSpace :: ReadP Char
whiteSpace = satisfy (`elem` " \t")

validIdentifier :: ReadP String
validIdentifier = do
    first <- validFirstCharacter
    rest  <- many validIdentifierCharacter
    return $ first : rest

validFirstCharacter :: ReadP Char
validFirstCharacter = alphabet

validIdentifierCharacter :: ReadP Char
validIdentifierCharacter = alphabet <|> digit <|> underscore

alphabet :: ReadP Char
alphabet = satisfy isAsciiLower

digit :: ReadP Char
digit = satisfy isDigit

underscore :: ReadP Char
underscore = char '_'

-- Helper routines for generating the build rules
makeSourceRuleMaps
    :: [String]
    -> ModuleSearchTree
    -> [SourceFile]
    -> ( Map.Map FilePath [FilePath]
       , Map.Map FilePath (IO [FilePath])
       , Map.Map FilePath (Action ())
       )
makeSourceRuleMaps = makeSpecialSourceRuleMaps Map.empty

makeSpecialSourceRuleMaps
    :: SpecialCommands
    -> [String]
    -> ModuleSearchTree
    -> [SourceFile]
    -> ( Map.Map FilePath [FilePath]
       , Map.Map FilePath (IO [FilePath])
       , Map.Map FilePath (Action ())
       )
makeSpecialSourceRuleMaps specialCommands flags moduleSearchTree sources =
    foldl
        (\(previousFilesMap, previousDependsMap, previousCommandMap) (newFilesMap, newDependsMap, newCommandMap) ->
            ( previousFilesMap `Map.union` newFilesMap
            , previousDependsMap `Map.union` newDependsMap
            , previousCommandMap `Map.union` newCommandMap
            )
        )
        (Map.empty, Map.empty, Map.empty)
        (map
            (makeSpecialSourceRuleMap specialCommands flags moduleSearchTree)
            sources
        )

makeSpecialSourceRuleMap
    :: SpecialCommands
    -> [String]
    -> ModuleSearchTree
    -> SourceFile
    -> ( Map.Map FilePath [FilePath]
       , Map.Map FilePath (IO [FilePath])
       , Map.Map FilePath (Action ())
       )
makeSpecialSourceRuleMap specialCommands flags moduleSearchTree source@(SourceFile src _ modulesContained)
    = let buildDir        = getNodeDirectory moduleSearchTree
          compDeps        = compileTimeDepends source moduleSearchTree
          objectFileName  = buildDir </> takeFileName src -<.> "o"
          moduleFileNames = map ((buildDir </>) . (-<.> "mod"))
              $ Set.toList modulesContained
          additionalIncludeFlags =
                  map ("-I" ++) (getAdditionalBuildDirs moduleSearchTree)
          filesMap = foldl
              (\previousMap newFile -> Map.insert
                  newFile
                  (objectFileName : moduleFileNames)
                  previousMap
              )
              Map.empty
              (objectFileName : moduleFileNames)
          dependsMap = Map.singleton objectFileName compDeps
          commandMap =
                  Map.singleton objectFileName
                      $ case Map.lookup src specialCommands of
                            Just command ->
                                cmd command ["-o", objectFileName, src] :: Action ()
                            Nothing ->
                                cmd compiler
                                    ["-c", "-J" ++ buildDir]
                                    additionalIncludeFlags
                                    flags
                                    ["-o", objectFileName, src] :: Action ()
      in  (filesMap, dependsMap, commandMap)

getAdditionalBuildDirs :: ModuleSearchTree -> [FilePath]
getAdditionalBuildDirs moduleSearchTree =
    let remainingNodes  = getRemainingNodes moduleSearchTree
        fromThisLevel   = map getNodeDirectory (Set.toList remainingNodes)
        fromLowerLevels = concatMap getAdditionalBuildDirs remainingNodes
    in  fromThisLevel ++ fromLowerLevels

makeExecutableMaps
    :: ModuleSearchTree
    -> [Program]
    -> (Set.Set FilePath, Map.Map FilePath (IO [FilePath]))
makeExecutableMaps moduleSearchTree programs = foldl
    (\(previousSet, previousMap) (newSet, newMap) ->
        (previousSet `Set.union` newSet, previousMap `Map.union` newMap)
    )
    (Set.empty, Map.empty)
    (map (makeExecutableMap moduleSearchTree) programs)

makeExecutableMap
    :: ModuleSearchTree
    -> Program
    -> (Set.Set FilePath, Map.Map FilePath (IO [FilePath]))
makeExecutableMap moduleSearchTree program =
    let
        buildDir   = getNodeDirectory moduleSearchTree
        executable = buildDir </> getProgramName program <.> exe
        linkDeps   = linkTimeDepends (getProgramSource program) moduleSearchTree
    in
        (Set.singleton executable, Map.singleton executable linkDeps)

compileTimeDepends :: SourceFile -> ModuleSearchTree -> IO [FilePath]
compileTimeDepends source moduleSearchTree = do
    others <- recursiveCompileTimeDepends source moduleSearchTree
    return $ getFileName source : others

recursiveCompileTimeDepends :: SourceFile -> ModuleSearchTree -> IO [FilePath]
recursiveCompileTimeDepends source moduleSearchTree = case source of
    SourceFile _ modulesUsed _ -> do
        modules <- foldM collect Set.empty modulesUsed
        return $ Set.toList modules
      where
        collect previousModules nextModule =
            if nextModule `Map.member` getNodeModules moduleSearchTree
                then
                    return
                    $            (   getNodeDirectory moduleSearchTree
                                 </> nextModule
                                 <.> "mod"
                                 )
                    `Set.insert` previousModules
                else do
                    moreMods <- foldM recursePart
                                      Set.empty
                                      (getRemainingNodes moduleSearchTree)
                    return $ moreMods `Set.union` previousModules
          where
            recursePart prev tree = do
                more <- recursiveCompileTimeDepends source tree
                return $ Set.fromList more `Set.union` prev

linkTimeDepends :: SourceFile -> ModuleSearchTree -> IO [FilePath]
linkTimeDepends source moduleSearchTree = do
    (fromModules, fromHeaders) <- recursiveLinkTimeDepends
        (Set.empty, Set.empty)
        source
        moduleSearchTree
    return $ Set.toList (fromModules `Set.union` fromHeaders)

recursiveLinkTimeDepends
    :: (Set.Set FilePath, Set.Set FilePath)
    -> SourceFile
    -> ModuleSearchTree
    -> IO (Set.Set FilePath, Set.Set FilePath)
recursiveLinkTimeDepends (fromModules, fromHeaders) source@(SourceFile _ modulesUsed _) moduleSearchTree
    = let obj =
                  getNodeDirectory moduleSearchTree
                      </>  (takeFileName . getFileName) source
                      -<.> "o"
          withCurrentObj = obj `Set.insert` fromModules
      in  if obj `Set.member` fromModules
              then return (fromModules, fromHeaders)
              else foldM (linkTimeModuleSearch moduleSearchTree)
                         (withCurrentObj, fromHeaders)
                         (Set.toList modulesUsed)

linkTimeModuleSearch
    :: ModuleSearchTree
    -> (Set.Set FilePath, Set.Set FilePath)
    -> ModuleName
    -> IO (Set.Set FilePath, Set.Set FilePath)
linkTimeModuleSearch moduleSearchTree (fromModules, fromHeaders) moduleName =
    case maybeModule of
        Just module' -> recursiveLinkTimeDepends (fromModules, fromHeaders)
                                                 (getModuleSource module')
                                                 moduleSearchTree
        Nothing -> foldM collect
                         (fromModules, fromHeaders)
                         (getRemainingNodes moduleSearchTree)
  where
    maybeModule = Map.lookup moduleName (getNodeModules moduleSearchTree)
    collect prevObjs nextSearchTree =
        linkTimeModuleSearch nextSearchTree prevObjs moduleName

removeIfExists :: FilePath -> IO ()
removeIfExists file = removeFile file `catch` handleExists
  where
    handleExists e | isDoesNotExistError e = return ()
                   | otherwise             = throwIO e

createDriver :: FilePath -> [FilePath] -> Action ()
createDriver driverFile collectionFiles =
    liftIO $ makeDriver driverFile collectionFiles
