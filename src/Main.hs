import Parser

ast :: String -> String
ast = show . parseProgram

main :: IO ()
main = getContents >>= putStrLn . ast

