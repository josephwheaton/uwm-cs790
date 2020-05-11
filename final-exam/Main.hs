import AST
import Eval
import Parser
import Data.Either

main :: IO ()
main = do
         let f = "test.txt"

         -- read a list of declarations 'd' from file 'f'
         d <- readFile f 

         -- run 'prog' parser to parse 'd' to obtain an AST
         let ast = runP prog d

         case ast of Left e  -> putStrLn $ show e   -- print parse error
                     Right x -> do 
                                  putStrLn $ show x -- print the AST 
                                  -- putStrLn $ pp x   -- pretty-print the AST 
                                  putStrLn $ runD x -- eval x and print the context or eval error

