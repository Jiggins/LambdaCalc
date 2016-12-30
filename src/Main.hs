module Main where

import Text.Parsec hiding (State)
import Text.Parsec.Error (errorMessages, messageString)

import Data.Functor.Identity
import Data.List (intercalate)

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map

import System.Console.Haskeline

import Eval
import Lambda
import Parser

showStep :: (MonadIO m, Show a) => (Int, a) -> InputT m ()
showStep (depth, exp) = outputStrLn ((replicate depth ' ') ++ "=> " ++ show exp)

interactive :: IO ()
interactive = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "λ > "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> parseExpression input >> loop

parseExpression :: MonadIO m => String -> InputT m ()
parseExpression input = case parse expression "<interactive>" input of
                    Left err  -> (outputStrLn . show) err
                    Right exp -> do
                      let (result, steps) = runEval exp
                      mapM_ showStep steps
                      outputStrLn . show $ result

parseString input = case parse expression "<interactive>" input of
                      Left err -> error . unwords . map messageString . errorMessages $ err
                      Right exp -> runEval exp

main :: IO ()
main = putStrLn "Hello, Haskell!"
