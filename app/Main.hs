{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Word8
import Data.List
import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Catch
import System.Environment (getArgs)
import System.IO (withFile, IOMode(..), Handle)
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
import qualified Streamly.Internal.Data.Array.Foreign.Type as F
import qualified Streamly.Internal.Data.Array.Foreign.Mut.Type as FM
import qualified Streamly.Internal.Data.Unfold.Type as Unfold
import qualified Data.DList as DL

import Streamly.External.ByteString.Lazy (toChunks)


main :: IO ()
main = do
  [file] <- getArgs
{--
  lbs <- L.readFile file
  let l = last $ DL.toList $ lines lbs
  putStrLn $ show l
--}

{--
  lbs <- L.readFile file
  (Right result) <- pure $ A.parseOnly newlineParserAtto lbs
  let l = last result
  putStrLn $ show l
--}

  lbs <- L.readFile file
  Just l <- P.last $ P.foldMany lineFold (P.unfoldMany read' $ toChunks lbs)
  putStrLn $ show (fmap w2c l)

 where
  -- attoparsec parser
  -- 0.7s
  newlineParserAtto :: A.Parser [BS.ByteString]
  newlineParserAtto = many $ A.takeWhileIncluding (/= _lf)


  -- streamly Fold
  -- 0.7s
  {-# INLINE lineFold #-}
  lineFold :: Applicative m => Fold.Fold m Word8 (DL.DList Word8)
  lineFold = Fold.Fold step initial extract
    where
      initial = pure $ Fold.Partial DL.empty
      step !s !a
        | a == _lf = pure $ Fold.Done s
        | otherwise = pure $ Fold.Partial (DL.snoc s a)
      extract !s = pure s


  -- ParserD
  -- 1s
  {-# INLINE newlineParserD #-}
  newlineParserD :: (MonadIO m, MonadThrow m) => ParserD.Parser m Word8 (DL.DList Word8)
  newlineParserD = ParserD.Parser step initial extract
    where
      initial = pure $ ParserD.IPartial DL.empty
      step s a
        | a == _lf = pure $ ParserD.Done 0 s
        | otherwise = pure $ ParserD.Partial 0 (DL.snoc s a)
      extract = pure

  -- bytestring functions
  -- 0.175s
  {-# INLINE lines #-}
  lines :: L.ByteString -> DL.DList (L.ByteString)
  lines = DL.unfoldr inner
   where
    {-# INLINE inner #-}
    inner input'
      | L.null input' = Nothing
      | otherwise =
          case L.elemIndex _lf input' of
            Nothing -> Just (input', L.empty)
            Just i -> 
              let (prefix, suffix) = L.splitAt i input'
              in Just (prefix, L.drop 1 suffix)


{-# INLINE read' #-}
read' :: Unfold.Unfold IO (F.Array Word8) Word8
read' = Unfold.lmap F.unsafeThaw FM.read

{-# INLINE fileStream #-}
fileStream :: Handle -> P.SerialT IO (F.Array Word8)
fileStream handle = P.unfold Handle.readChunks handle
