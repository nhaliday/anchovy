module Main where

import qualified Anchovy.Parser as P

import Control.Monad.Trans (liftIO)
import qualified System.Console.Haskeline as H

process :: String -> IO ()
process line =
	let res = P.parseToplevel line
	in case res of
		Left err -> print err
		Right exprs -> mapM_ print exprs

main :: IO ()
main = H.runInputT H.defaultSettings loop
	where loop = do
		inp <- H.getInputLine "ready> "
		case inp of
			Nothing -> H.outputStrLn "Goodbye."
			Just line -> (liftIO $ process line) >> loop
