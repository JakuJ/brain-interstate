import Parse (Node, parseProgram)

testParser :: Bool -> String -> IO ()
testParser target str = do
    case parseProgram str of
        Right _ -> if target then success else failure >> fail "Test case should fail"
        Left err -> if not target then success else failure >> fail err
    where
        success = putStrLn $ "TRYING \""++ str ++ "\"\nSUCCESS"
        failure = putStrLn $ "TRYING \"" ++ str ++ "\"\nFAILURE"

goodSyntaxTestCases, badSyntaxTestCases :: [String]
goodSyntaxTestCases = ["", "+-.,<>", "+++[+]---", "+[>><++-,][+][-]", "+++ [.] -., comment"]
badSyntaxTestCases = ["[", "]", "+++[--[..", "+[+[]]-]-", "[]"]

main :: IO ()
main = do
    putStrLn "" >> putStrLn "Valid syntax tests"
    mapM_ (testParser True) goodSyntaxTestCases
    putStrLn "" >> putStrLn "Invalid syntax tests"
    mapM_ (testParser False) badSyntaxTestCases
    putStrLn "" >> putStrLn "All done"
