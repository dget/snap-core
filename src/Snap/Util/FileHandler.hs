{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Snap.Util.FileHandler where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.ByteString (ByteString)
import           Data.ByteString.Internal (c2w, w2c)
import           Data.Char
import           Data.Int
import           Data.Word (Word8)
import           Prelude hiding (take, takeWhile, getLine)
------------------------------------------------------------------------------
import           Snap.Internal.Debug
import           Control.Monad.Trans (lift)
import           Data.Attoparsec hiding (many, Result(..))
import qualified Data.Attoparsec as A
import           Data.Attoparsec.Enumerator
----
import           Snap.Iteratee hiding (map)
import           Snap.Internal.Iteratee.Debug
import           Snap.Types hiding (headers)
import           Snap.Util.KmpEnumeratee


-----------------------------------------------------------------------------
type ParamName   = ByteString
type FileName    = ByteString
type ContentType = ByteString
type ParamBody   = ByteString

data FileInfo  = FileInfo FileName ContentType
     deriving Prelude.Show
data ParamHeaders = ParamInfo { name     :: ParamName,
                                fileInfo :: (Maybe FileInfo) }
     deriving Prelude.Show

------------------------------------------------------------------------------
bUFSIZ :: Int
bUFSIZ = 8192


handleFileUpload :: MonadSnap m => (ParamHeaders -> Iteratee ByteString IO a) -> m [a]
handleFileUpload f = do
     req <- getRequest
     case multipartBoundary req of
       Nothing -> return []
       Just x  -> do
         runRequestBody $ internalHandleMultipart x f

multipartBoundary :: Request -> Maybe S.ByteString
multipartBoundary req = do 
  x <- getHeader "Content-Type" req
  return $ S.drop 21 x

-- parser
sp, digit, letter :: Parser Word8
sp       = word8 $ c2w ' '
digit    = satisfy (isDigit . w2c)
letter   = satisfy (isAlpha . w2c)

char :: Char -> Parser Word8
char x   = word8 $ c2w x

isEndOfLine :: Word8 -> Bool
isEndOfLine w = (w == c2w '\n' || w == c2w '\r')

eol :: Parser ByteString
eol = (string "\n") <|> (string "\r\n")

crlf :: Parser ByteString
crlf = string "\r\n"

pBoundary :: ByteString -> Parser Bool
pBoundary boundary = try $ do
  string "--"
  string boundary
  (eol *> pure False) <|> (string "--" *> pure True)
  
getLine :: Parser ByteString
getLine =  takeWhile (not . isEndOfLine) <* eol

takeLine :: Parser ()
takeLine = getLine *> pure ()

parseFirstBoundary :: ByteString -> Parser Bool
parseFirstBoundary b = pBoundary b <|> (takeLine *> parseFirstBoundary b)

untilChar :: Char -> Parser ByteString
untilChar c = takeTill (\x -> x == (c2w c)) 

pName :: Parser ParamName
pName = (string "Content-Disposition: form-data; name=\"") *> (untilChar '"') <* (word8 (c2w '"'))

pFileName :: Parser FileName
pFileName = (string "filename=\"") *> (untilChar '"') <* (word8 (c2w '"'))

pContentType :: Parser ContentType
pContentType = (string "Content-Type: ") *> getLine 

pFileInfo :: Parser FileInfo
pFileInfo = FileInfo <$> ((string "; ") *> pFileName <* eol) <*>
                         pContentType

pmFileName :: Parser (Maybe FileInfo)
pmFileName = (Just <$> pFileInfo) <|> (eol *> pure Nothing)

pParamHeaders :: Parser ParamHeaders
pParamHeaders = ParamInfo <$> pName <*>
                             pmFileName <* eol

grabPart :: (Monad m) => Enumeratee MatchInfo ByteString m a
grabPart = checkDone go
  where
    go :: (Monad m) => (Stream ByteString -> Iteratee ByteString m a) 
                    -> Iteratee MatchInfo m (Step ByteString m a)
    go k =
      Snap.Iteratee.head >>= maybe (finish k) (process k)

    -- called when outer stream is EOF
    finish :: (Monad m) => (Stream ByteString -> Iteratee ByteString m a) 
                        -> Iteratee MatchInfo m (Step ByteString m a)
    finish k = lift $ runIteratee $ k EOF

    -- no match ==> pass the stream chunk along
    process :: (Monad m) => (Stream ByteString -> Iteratee ByteString m a) 
                         -> MatchInfo
                         -> Iteratee MatchInfo m (Step ByteString m a)
    process k (NoMatch s) = do
      step <- lift $ runIteratee $ k $ Chunks [s]
      checkDone go step

    process k (Match _) = lift $ runIteratee $ k EOF

grabParts :: (Monad m) => Iteratee ByteString m a
                       -> Iteratee MatchInfo m [a]
grabParts partIter = do
  let iter = partIter >>= \x -> skipToEof >> return x
  go iter []
  where
    go :: (Monad m) => Iteratee ByteString m a 
                    -> [a] -- replace with DList
                    -> Iteratee MatchInfo m [a]
    go iter soFar = do
      b <- isEOF
      if b
        then return soFar
        else do
          -- step :: Step ByteString m a
          step <- lift $ runIteratee iter

          -- grabPart step :: Iteratee MatchInfo m (Step ByteString m a)
          innerStep <- grabPart step

          -- output :: a 
          output <- lift $ run_ $ returnI innerStep

          -- and recurse
          go iter (output : soFar)


internalHandleMultipart :: (MonadIO m) => ByteString -- boundary
                                     -> (ParamHeaders -> Iteratee ByteString m a)
                                     -> Iteratee ByteString m [a]
internalHandleMultipart bound clientHandler = do
  -- kmpEnumeratee bound :: Enumeratee ByteString MatchInfo m1 a1
  -- runIteratee $ grabParts clientIter :: m (Step MatchInfo m [a])
  partsStep <- lift $ runIteratee $ grabParts $ compressIteratee clientHandler
  enumStep  <- iterateeDebugWrapper "kmp" $ kmpEnumeratee fullBound partsStep

  output <- lift $ run_ $ returnI enumStep
  return output

  where 
    fullBound = S.concat ["--", bound, "\n"]

    compressIteratee :: (MonadIO m) => (ParamHeaders -> Iteratee ByteString m a) -> Iteratee ByteString m a
    compressIteratee handler = do
--      headers <- iterateeDebugWrapper "header parser" $ iterParser pParamHeaders
      handler $ ParamInfo "input1" Nothing

  
------
testHandler :: ParamHeaders -> Iteratee ByteString IO [ByteString]
testHandler _ = consume

--
testStr :: ByteString
testStr = S.pack $ map c2w $ unlines [     
        "------WebKitFormBoundaryoagG1yA8o4dcYBL2",
        "Content-Disposition: form-data; name=\"input1\"",
        "",
        "hey",
        "------WebKitFormBoundaryoagG1yA8o4dcYBL2",
        "Content-Disposition: form-data; name=\"input2\"",
        "",
        "you",
        "------WebKitFormBoundaryoagG1yA8o4dcYBL2",
        "Content-Disposition: form-data; name=\"input3\"",
        "",
        "guys",
        "------WebKitFormBoundaryoagG1yA8o4dcYBL2",
        "Content-Disposition: form-data; name=\"file1\"; filename=\"test4.txt\"",
        "Content-Type: text/plain",
        "",
        "21",
        "",
        "------WebKitFormBoundaryoagG1yA8o4dcYBL2--"
        ]       

testHand :: Iteratee ByteString IO [[ByteString]]
testHand =  internalHandleMultipart "----WebKitFormBoundaryoagG1yA8o4dcYBL2" testHandler

s2b x = S.pack $ map c2w x

--testAct :: (MonadSnap m) => m [L.ByteString]
testAct = do
  step <- liftIO $ runIteratee testHand
  enumBS testStr step
  