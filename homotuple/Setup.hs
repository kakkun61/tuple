import           Prelude hiding (head, init, last, reverse, tail)
import qualified Prelude

import Data.Char                          (isDigit)
import Data.Foldable                      (for_)
import Data.List                          (intercalate, intersperse, isPrefixOf, replicate, stripPrefix)
import Distribution.Simple                (Args, UserHooks (preBuild), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.Setup          (BuildFlags)
import Distribution.Types.HookedBuildInfo (HookedBuildInfo, emptyHookedBuildInfo)
import System.Directory                   (copyFile, createDirectoryIfMissing, getTemporaryDirectory, removeFile)
import System.IO                          (Handle, IOMode (ReadMode), hClose, hGetLine, hIsEOF, hPutStrLn,
                                           hSetNewlineMode, noNewlineTranslation, openTempFile, stdin, withFile)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { preBuild = preProcessListTuple }

preProcessListTuple :: Args -> BuildFlags -> IO HookedBuildInfo
preProcessListTuple _ _ = do
  let
    dir = "src/Data/Tuple"
    file = "Homotuple.hs"
    srcPath = dir ++ "/" ++ file
    templatePath = "template/Homotuple.hs"
    templateItemPath = "template/HomotupleItem.hs"
  tempPath <-
    withFile templatePath ReadMode $ \template -> do
      tempDir <- (++ "/homotuple") <$> getTemporaryDirectory
      createDirectoryIfMissing True tempDir
      (tempPath, temp) <- openTempFile tempDir file
      putStrLn $ "temporaly file: " ++ tempPath
      hSetNewlineMode template noNewlineTranslation
      hSetNewlineMode temp noNewlineTranslation
      hSetNewlineMode stdin noNewlineTranslation
      templateItem <- lines <$> readFile templateItemPath
      loop template temp templateItem
      hClose temp
      pure tempPath
  copyFile tempPath srcPath
  removeFile tempPath
  pure emptyHookedBuildInfo
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
      , let n = read $ takeWhile isDigit rest
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
        n = fromIntegral l
        homotuple = paren $ replicate n "a"
        tuple = paren $ take n i012
        list = bracket $ take n i012
        length = show l
        paren xs = "(" ++ intercalate ", " xs ++ ")"
        bracket xs = "[" ++ intercalate ", " xs ++ "]"
        i012 = ('i':) . show <$> [0 ..]
