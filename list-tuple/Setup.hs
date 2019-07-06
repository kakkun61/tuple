import           Prelude                            hiding (head, init, last,
                                                     tail)
import qualified Prelude

import           Data.Char                          (isDigit)
import           Data.Foldable                      (for_)
import           Data.List                          (intercalate, isPrefixOf,
                                                     replicate, stripPrefix)
import           Distribution.Simple                (Args, UserHooks (preBuild),
                                                     defaultMainWithHooks,
                                                     simpleUserHooks)
import           Distribution.Simple.Setup          (BuildFlags)
import           Distribution.Types.HookedBuildInfo (HookedBuildInfo,
                                                     emptyHookedBuildInfo)
import           System.Directory                   (copyFile, removeFile)
import           System.IO                          (Handle, IOMode (ReadMode),
                                                     hClose, hGetLine, hIsEOF,
                                                     hPutStrLn, hSetNewlineMode,
                                                     noNewlineTranslation,
                                                     openTempFile, withFile)

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
  tempPath <-
    withFile srcPath ReadMode $ \src -> do
      (tempPath, temp) <- openTempFile dir file
      hSetNewlineMode src noNewlineTranslation
      hSetNewlineMode temp noNewlineTranslation
      loop src temp
      hClose temp
      pure tempPath
  copyFile tempPath srcPath
  removeFile tempPath
  pure emptyHookedBuildInfo
  where
    loop :: Handle -> Handle -> IO ()
    loop src temp =
      go
      where
        go = do
          eof <- hIsEOF src
          if eof
            then pure ()
            else do
              line <- hGetLine src
              for_ (preprocess line) (hPutStrLn temp)
              loop src temp

    preprocess :: String -> [String]
    preprocess t
      | Just rest <- stripPrefix "---- embed " t
      , let n = read $ takeWhile isDigit rest
      = embed n template
      | otherwise = [t]

    embed :: Word -> [String] -> [String]
    embed l t
      | l >= 3 = go <$> t
      | otherwise = error "length must be larger than or equal to 3"
      where
        go "" = ""
        go t
          | Just rest <- stripPrefix "<tuple>" t = tuple ++ go rest
          | Just rest <- stripPrefix "<cons>" t = cons ++ go rest
          | Just rest <- stripPrefix "<tail>" t = tail ++ go rest
          | Just rest <- stripPrefix "<init>" t = init ++ go rest
          | Just rest <- stripPrefix "<last>" t = last ++ go rest
          | Just rest <- stripPrefix "<length>" t = length ++ go rest
          | Just rest <- stripPrefix "<tuple-head>" t = tupleHead ++ go rest
          | Just rest <- stripPrefix "<tuple-tail>" t = tupleTail ++ go rest
          | Just rest <- stripPrefix "<tuple-init>" t = tupleInit ++ go rest
          | Just rest <- stripPrefix "<tuple-last>" t = tupleLast ++ go rest
          | Just rest <- stripPrefix "<cons>" t = cons ++ go rest
          | Just rest <- stripPrefix "<" t = error $ "unknown tag: " ++ takeWhile (/= '>') rest
          | (s, rest) <- span (/= '<') t = s ++ go rest
        n = fromIntegral l
        m = n - 1
        tuple = paren $ take n abc
        tail = paren $ take m $ Prelude.tail abc
        init = paren $ take m abc
        last = [last']
        length = show l
        tupleHead = paren $ take n $ 'a' : unders
        tupleTail = paren $ take n $ '_' : Prelude.tail abc
        tupleInit = paren $ reverse $ '_' : reverse (take m abc)
        tupleLast = paren $ reverse $ take n $ last' : unders
        cons = "(" ++ replicate m ',' ++ ")"
        paren xs = "(" ++ intercalate ", " ((: []) <$> xs) ++ ")"
        abc = ['a' ..]
        unders = repeat '_'
        last' = abc !! m

    template :: [String]
    template =
      [ "-- <length>"
      , ""
      , "type instance Cons a <tail> = <tuple>"
      , "type instance Head <tuple> = a"
      , "type instance Tail <tuple> = <tail>"
      , "type instance Init <tuple> = <init>"
      , "type instance Last <tuple> = <last>"
      , "type instance Length <tuple> = <length>"
      , ""
      , "instance HasHead' <tuple> a where"
      , "  head' <tuple-head> = a"
      , ""
      , "instance HasTail' <tuple> <tail> where"
      , "  tail' <tuple-tail> = <tail>"
      , ""
      , "instance HasInit' <tuple> <init> where"
      , "  init' <tuple-init> = <init>"
      , ""
      , "instance HasLast' <tuple> <last> where"
      , "  last' <tuple-last> = <last>"
      , ""
      , "instance HasCons' <tuple> a <tail> where"
      , "  cons' a <tail> = <tuple>"
      , ""
      , "instance HasUncons' <tuple> a <tail> where"
      , "  uncons' <tuple> = (a, <tail>)"
      , ""
      , "instance HasHead <tuple>"
      , ""
      , "instance HasTail <tuple>"
      , ""
      , "instance HasInit <tuple>"
      , ""
      , "instance HasLast <tuple>"
      , ""
      , "instance HasCons a <tail>"
      , ""
      , "instance HasUncons <tuple>"
      , ""
      , "instance HasLength <tuple>"
      , ""
      , "{-# COMPLETE Cons' :: <cons> #-}"
      , "{-# COMPLETE Cons :: <cons> #-}"
      ]
