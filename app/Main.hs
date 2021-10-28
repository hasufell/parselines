module Main where

import Data.Word8
import Data.List
import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import System.Environment (getArgs)
import System.IO (withFile, IOMode(..))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Internal (w2c)


import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString.Lazy as A

import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.Parser.ParserD as ParserD
import qualified Streamly.Internal.Data.Parser.ParserK.Type as ParserK
import qualified Streamly.Internal.Data.Fold.Type as Fold
import qualified Streamly.Prelude as P
import qualified Streamly.Internal.Data.Stream.IsStream.Eliminate as E
import qualified Streamly.Internal.Data.Stream.IsStream.Reduce as R
import qualified Streamly.FileSystem.Handle as Handle


main :: IO ()
main = do
  [file] <- getArgs

  -- attoparsec
  lbs <- L.readFile file
  (Right result) <- pure $ A.parseOnly newlineParserAtto lbs
  let l = length result



  -- streamly
{--
  withFile file ReadMode $ \handle -> do
    -- ParserK
    -- l <- P.length $ R.parseMany newlineParserK (fileStream handle)

    -- ParserD
    l <- P.length $ R.parseManyD newlineParserD (fileStream handle)

--}
  putStrLn $ show l

 where
  {-# INLINE fileStream #-}
  fileStream handle = P.unfold Handle.read handle
  {-# INLINE newlineParserK #-}
  newlineParserK :: MonadCatch m => ParserK.Parser m Word8 [Word8]
  newlineParserK =  Parser.takeWhile (/= _lf) Fold.toList <* (ParserK.toParserK next)

  {-# INLINE newlineParserD #-}
  newlineParserD :: MonadThrow m => ParserD.Parser m Word8 [Word8]
  newlineParserD = ParserD.takeWhile (/= _lf) Fold.toList <* next

  newlineParserAtto :: A.Parser [BS.ByteString]
  newlineParserAtto = many $ A.takeWhileIncluding (/= _lf)

{-# INLINE next #-}
next :: Monad m => ParserD.Parser m a (Maybe a)
next = ParserD.Parser step initial extract
  where
  initial = pure $ ParserD.IPartial ()
  step _ a = pure $ ParserD.Done 0 (Just a)
  extract _ = pure Nothing
