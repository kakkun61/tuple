import           Prelude          (Bool (True), IO, Int, Maybe (Just),
                                   show, String, Word, concatMap, error,
                                   fromIntegral, lines, otherwise, pure,
                                   putStrLn, read, readFile, span, take,
                                   takeWhile, ($), (++), (.), (/=), (<$>), (>=), FilePath)
import qualified Prelude

import           Data.Char        (isDigit)
import           Data.Foldable    (for_)
import           Data.List        (intercalate, replicate, stripPrefix)
import           System.Directory (copyFile, createDirectoryIfMissing,
                                   getTemporaryDirectory, removeFile)
import           System.IO        (Handle, IOMode (ReadMode), hClose, hGetLine,
                                   hIsEOF, hPutStrLn, hSetNewlineMode,
                                   noNewlineTranslation, openTempFile, stdin,
                                   withFile, stderr)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [wd] -> app wd 
    _ -> do
      hPutStrLn stderr "Usage: homotuple-template DIR"
      exitFailure

app :: FilePath -> IO ()
app wd = do      
  let
    dir = wd ++ "/" ++ "src/Data/Tuple"
    file = "Homotuple.hs"
    srcPath = dir ++ "/" ++ file
    templatePath = wd ++ "/" ++ "template/Homotuple.hs"
    templateItemPath = wd ++ "/" ++ "template/HomotupleItem.hs"
  tempPath <-
    withFile templatePath ReadMode $ \template -> do
      tempDir <- (++ "/homotuple") <$> getTemporaryDirectory
      createDirectoryIfMissing True tempDir
      (tempPath, temp) <- openTempFile tempDir file
      putStrLn $ "temporary file: " ++ tempPath
      hSetNewlineMode template noNewlineTranslation
      hSetNewlineMode temp noNewlineTranslation
      hSetNewlineMode stdin noNewlineTranslation
      templateItem <- lines <$> readFile templateItemPath
      loop template temp templateItem
      hClose temp
      pure tempPath
  createDirectoryIfMissing True dir
  copyFile tempPath srcPath
  removeFile tempPath
  where
    loop :: Handle -> Handle -> [String] -> IO ()
    loop template temp templateItem =
      go
      where
        go = do
          eof <- hIsEOF template
          if eof
            then pure ()
            else do
              line <- hGetLine template
              for_ (preprocess line templateItem) (hPutStrLn temp)
              go

    preprocess :: String -> [String] -> [String]
    preprocess line templateItem
      | Just rest <- stripPrefix "---- embed " line
      , let n :: Word
            n = read $ takeWhile isDigit rest
      = embed n templateItem
      | otherwise = [line]

    embed :: Word -> [String] -> [String]
    embed l templateItem
      | l >= 2 = concatMap go templateItem
      | otherwise = error "length must be larger than or equal to 2"
      where
        go "" = [""]
        go t
          | Just rest <- stripPrefix "<homotuple>" t = [homotuple ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<tuple>" t = [tuple ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<list>" t = [list ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<length>" t = [length ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<" t = error $ "unknown tag: " ++ takeWhile (/= '>') rest
          | (s, rest) <- span (/= '<') t = [s ++ Prelude.head (go rest)]
        n :: Int
        n = fromIntegral l
        homotuple = paren $ replicate n "a"
        tuple = paren $ take n i012
        list = bracket $ take n i012
        length = show l
        paren xs = "(" ++ intercalate ", " xs ++ ")"
        bracket xs = "[" ++ intercalate ", " xs ++ "]"
        i012 = ('i':) . show <$> [0 :: Word ..]
