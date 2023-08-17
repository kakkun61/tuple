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
    file = "List.hs"
    srcPath = dir ++ "/" ++ file
    templatePath = "template/List.hs"
    templateItemPath = "template/ListItem.hs"
    templateAtPath = "template/ListAt.hs"
  tempPath <-
    withFile templatePath ReadMode $ \template -> do
      tempDir <- (++ "/list-tuple") <$> getTemporaryDirectory
      createDirectoryIfMissing True tempDir
      (tempPath, temp) <- openTempFile tempDir file
      putStrLn $ "temporary file: " ++ tempPath
      hSetNewlineMode template noNewlineTranslation
      hSetNewlineMode temp noNewlineTranslation
      hSetNewlineMode stdin noNewlineTranslation
      templateItem <- lines <$> readFile templateItemPath
      templateAt <- lines <$> readFile templateAtPath
      loop template temp templateItem templateAt
      hClose temp
      pure tempPath
  copyFile tempPath srcPath
  removeFile tempPath
  pure emptyHookedBuildInfo
  where
    loop :: Handle -> Handle -> [String] -> [String] -> IO ()
    loop template temp templateItem templateAt =
      go
      where
        go = do
          eof <- hIsEOF template
          if eof
            then pure ()
            else do
              line <- hGetLine template
              for_ (preprocess line templateItem templateAt) (hPutStrLn temp)
              go

    preprocess :: String -> [String] -> [String] -> [String]
    preprocess line templateItem templateAt
      | Just rest <- stripPrefix "---- embed " line
      , let n = read $ takeWhile isDigit rest
      = embed n templateItem templateAt
      | otherwise = [line]

    embed :: Word -> [String] -> [String] -> [String]
    embed l templateItem templateAt
      | l >= 3 = concatMap go templateItem
      | otherwise = error "length must be larger than or equal to 3"
      where
        go "" = [""]
        go t
          | Just rest <- stripPrefix "<tuple>" t = [tuple ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<cons>" t = [cons ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<tail>" t = [tail ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<init>" t = [init ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<last>" t = [last ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<length>" t = [length ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<tuple-head>" t = [tupleHead ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<tuple-tail>" t = [tupleTail ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<tuple-init>" t = [tupleInit ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<tuple-last>" t = [tupleLast ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<reverse>" t = [reverse ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<" t = error $ "unknown tag: " ++ takeWhile (/= '>') rest
          | Just _ <- stripPrefix "---- has-at" t
          = concatMap go $ concat $ intersperse [""] $ embedAt <$> [0 .. l - 1]
          | Just rest <- stripPrefix "-" t = ["-" ++ Prelude.head (go rest)]
          | (s, rest) <- span ((&&) <$> (/= '<') <*> (/= '-')) t = [s ++ Prelude.head (go rest)]
        n = fromIntegral l
        m = n - 1
        tuple = paren $ take n i012
        tail = paren $ take m $ Prelude.tail i012
        init = paren $ take m i012
        last = i012 !! m
        length = show l
        tupleHead = paren $ take n $ "i0" : unders
        tupleTail = paren $ take n $ "_" : Prelude.tail i012
        tupleInit = paren $ Prelude.reverse $ "_" : zy
        tupleLast = paren $ Prelude.reverse $ take n $ last : unders
        cons = "(" ++ replicate m ',' ++ ")"
        paren xs = "(" ++ intercalate ", " xs ++ ")"
        i012 = ('i':) . show <$> [0 ..]
        unders = repeat "_"
        zy = Prelude.reverse (take m i012)
        zyx = Prelude.reverse (take n i012)
        reverse = paren $ zyx

        embedAt :: Word -> [String]
        embedAt at =
          go <$> templateAt
          where
            go "" = ""
            go t
              | Just rest <- stripPrefix "<at>" t = show at ++ go rest
              | Just rest <- stripPrefix "<item>" t = item ++ go rest
              | Just rest <- stripPrefix "<tuple-at>" t = tupleAt ++ go rest
              | Just rest <- stripPrefix "<" t = "<" ++ go rest
              | (s, rest) <- span (/= '<') t = s ++ go rest
            at' = fromIntegral at
            item = i012 !! at'
            tupleAt = paren $ take at' unders ++ [item] ++ take (n - at' - 1) unders
