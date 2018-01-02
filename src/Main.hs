module Main (main) where

import Control.Monad (when)
import System.Directory (doesFileExist, getPermissions, readable, createDirectoryIfMissing)
import System.Environment
import System.Exit (exitSuccess, exitWith, ExitCode (ExitFailure))
import System.IO
import Filesystem.Path.CurrentOS (decodeString, encodeString, directory)

import G4ipProver.Parser
import G4ipProver.Prover
import G4ipProver.LaTeXExporter


data Args = Args {
  proposition :: (Maybe String),
  help :: Bool,
  proofFile :: String,
  contextFile :: String,
  proofHeader :: String,
  proofFooter :: String,
  contextHeader :: String,
  contextFooter :: String,
  argParseError :: Bool,
  startREPL :: Bool
  }
  deriving (Show)


defaultArgs = Args {
  proposition = Nothing,
  help = False,
  proofFile = "output/proof.tex",
  contextFile = "output/context.tex",
  proofHeader = "static/proof-header.txt",
  proofFooter = "static/proof-footer.txt",
  contextHeader = "static/context-header.txt",
  contextFooter = "static/context-footer.txt",
  argParseError = False,
  startREPL = False
  }


parseArgs :: Args -> [String] -> Args
parseArgs o [] = o
parseArgs o [x] =
  o { proposition = Just x }
parseArgs o ("--help":xs) =
  parseArgs (o { help = True }) xs
parseArgs o ("-h":xs) =
  parseArgs (o { help = True }) xs
parseArgs o ("--proof-header":f:xs) =
  parseArgs (o { proofHeader = f }) xs
parseArgs o ("--proof-footer":f:xs) =
  parseArgs (o { proofFooter = f }) xs
parseArgs o ("--context-header":f:xs) =
  parseArgs (o { contextHeader = f }) xs
parseArgs o ("--context-footer":f:xs) =
  parseArgs (o { contextFooter = f }) xs
parseArgs o ("--proof-file":f:xs) =
  parseArgs (o { proofFile = f }) xs
parseArgs o ("--context-file":f:xs) =
  parseArgs (o { contextFile = f }) xs
parseArgs o _ = o { argParseError = True }


readFileIfExists :: String -> IO String
readFileIfExists path = do
  exists <- doesFileExist path
  if exists then do
    isReadable <- readable <$> getPermissions path
    if isReadable
    then readFile path
    else do
      putStrLn $ "Warning: file is not readable: " ++ path ++ ", ignoring"
      return ""
  else do
    putStrLn $ "Warning: file does not exist: " ++ path ++ ", ignoring"
    return ""


writeFileForcibly file content = do
  createDirectoryIfMissing True . encodeString . directory $ decodeString file
  writeFile file content


process opts input =
  case parseProp input of
    Right prop -> do
      putStrLn $ "Proposition: " ++ show prop
      case prove prop of
        Just proofTree -> do
          pHeader <- readFileIfExists (proofHeader opts)
          pFooter <- readFileIfExists (proofFooter opts)
          cHeader <- readFileIfExists (contextHeader opts)
          cFooter <- readFileIfExists (contextFooter opts)
          writeFileForcibly (proofFile opts) (pHeader ++ proofToString proofTree ++ pFooter)
          writeFileForcibly (contextFile opts) (cHeader ++ exportContexts proofTree ++ cFooter)
          putStrLn $ "Proof written to: " ++ (proofFile opts)
          putStrLn $ "Context list written to: " ++ (contextFile opts)
        Nothing -> putStrLn "Not provable."
    Left err -> do
      putStrLn $ "Incorrect proposition: " ++ err
      when (not $ startREPL opts) $ exitWith (ExitFailure 2)


printUsage = putStrLn $
      "USAGE\n" ++
      "g4ip-prover [OPTIONS] PROPOSITION\n" ++
      "g4ip-prover [OPTIONS]\n\nOPTIONS\n" ++
      "  --proof-header <file>       Template header file for proof.\n" ++
      "  --proof-footer <file>       Template footer file for proof.\n" ++
      "  --context-header <file>     Template header file for context.\n" ++
      "  --context-footer <file>     Template footer file for context.\n" ++
      "  --proof-file <file>         Proof file name. If the file exists, it will be overwritten.\n" ++
      "  --context-file <file>       Context file name. If the file exists, it will be overwritten.\n\n" ++
      "PROPOSITION syntax\n" ++
      "  Variables must consist of lower-case english characters and numbers.\n" ++
      "  Propositional connectives (with precedence level):\n" ++
      "    ~ , -    - negation, 1\n" ++
      "    /\\, &    - conjunction, 2\n" ++
      "    \\/, |    - disjunction, 3\n" ++
      "    ->, =>   - implication, 4\n" ++
      "    <-, <=   - implication, 4\n" ++
      "    <->, <=> - equivalency, 5\n" ++
      "  Logical constants:\n" ++
      "    T - True\n" ++
      "    F - False"


repl opts = do
  putStr "g4ip> "
  hFlush stdout
  prop <- getLine
  when (prop `elem` ["exit", "quit", ":q"]) exitSuccess
  process (opts { proposition = Just prop }) prop
  repl opts


main = do
  args <- getArgs
  let opts = parseArgs defaultArgs args
  when (argParseError opts) (printUsage >> exitWith (ExitFailure 1))
  when (help opts) (printUsage >> exitSuccess)
  case proposition opts of
    Just prop -> process opts prop
    Nothing -> do
      putStrLn "Welcome to g4ip-prover! Type \"exit\" to exit."
      repl opts { startREPL = True }
