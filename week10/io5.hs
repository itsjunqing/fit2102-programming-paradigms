-- Useful IO functions
import System.IO

-- Text input
-- getLine :: IO String
-- readFile :: FilePath -> IO String
-- read :: Read a => String -> a    (e.g. String -> Int)
-- lines :: String -> [String]

-- Text output
-- putStrLn :: String -> IO ()
-- print :: (Show a) => a -> IO ()
-- writeFile :: FilePath -> String -> IO ()

-- Exercise: sum the numbers in a file, one per line.
--   sumFile :: FilePath -> IO Int

-- (answer below)
sumFile :: FilePath -> IO Int
sumFile file = do 
  x <- readFile file 
  let y = sum $ fmap read $ lines x
  return y 




-- sumfile "test-file"



















-- sumFile :: FilePath -> IO Integer
-- sumFile path = do
--   contents <- readFile path
--   let strings = lines contents
--   let numbers = map read strings
--   let total = sum numbers
--   return total

-- sumFile2 =
--   fmap (sum . map read . lines) . readFile

-- sumFile3 path =
--   sum . map read . lines <$> readFile path

-- sumFile4 :: FilePath -> IO Integer
-- sumFile4 path = do
--   numbers <- map read . lines <$> readFile path
--   return (sum numbers)


-- program = do
--   putStrLn "enter file name:"
--   path <- getLine
--   total <- sumFile path
--   print total
--   program
