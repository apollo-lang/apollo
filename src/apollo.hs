import Parser
import Expr

ast :: String -> String
ast input = show (parseProgram input)

main :: IO ()
main = do
    input <- getContents
    putStrLn $ ast input
