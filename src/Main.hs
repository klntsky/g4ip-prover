module Main (main) where

import Control.Monad (when)
import System.Environment
import System.Exit (exitSuccess, exitWith, ExitCode (ExitFailure))
import System.IO

import Parser
import Prover
import LaTeXExporter


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


process opts input =
  case parseProp input of
    Right prop -> do
      putStrLn $ "Proposition: " ++ show prop
      case prove prop of
        Just proofTree -> do
          pHeader <- readFile (proofHeader opts)
          pFooter <- readFile (proofFooter opts)
          cHeader <- readFile (contextHeader opts)
          cFooter <- readFile (contextFooter opts)
          writeFile (proofFile opts) (pHeader ++ proofToString proofTree ++ pFooter)
          writeFile (contextFile opts) (cHeader ++ exportContexts proofTree ++ cFooter)
          putStrLn $ "Proof written to: " ++ (proofFile opts)
          putStrLn $ "Context list written to: " ++ (contextFile opts)
        Nothing -> putStrLn "Not provable."
    Left err -> do
      putStrLn $ "Incorrect proposition: " ++ err
      when (not $ startREPL opts) $ exitWith (ExitFailure 2)


printUsage = putStrLn usage
  where
    usage =
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
      "  Variables are lower-case english words or characters.\n" ++
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
  when (prop == "exit") exitSuccess
  process (opts { proposition = Just prop }) prop
  repl opts


main = do
  args <- getArgs
  let opts = parseArgs defaultArgs args
  when (argParseError opts) (printUsage >> exitWith (ExitFailure 1))
  when (help opts) (printUsage >> exitSuccess)
  case proposition opts of
    Just prop -> process opts prop
    Nothing -> repl opts { startREPL = True }
