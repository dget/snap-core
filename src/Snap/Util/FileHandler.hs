{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- Bugs:
-- Doesn't take files without content-type
-- Doesn't do multipart/mixed

module Snap.Util.FileHandler where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import qualified Data.ByteString as S
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
     debug $ show $ multipartBoundary req
     case multipartBoundary req of
       Nothing -> return []
       Just x  -> do
         runRequestBody $ internalHandleMultipart x f

multipartBoundary :: Request -> Maybe S.ByteString
multipartBoundary req = do 
  x <- getHeader "Content-Type" req
  return $ S.drop 30 x

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

pBoundary :: ByteString -> Parser ByteString
pBoundary boundary = try $ do
  _ <-string "--"
  string boundary
  
getLine :: Parser ByteString
getLine =  takeWhile (not . isEndOfLine) <* eol

takeLine :: Parser ()
takeLine = getLine *> pure ()


parseFirstBoundary :: ByteString -> Parser ByteString
parseFirstBoundary b = pBoundary b <|> (takeLine *> parseFirstBoundary b)

pLastBoundary :: ByteString -> Parser ByteString
pLastBoundary b = try $ do
  _ <- string "--"
  _ <- string b
  string "--"

pBoundaryEnd :: Parser Bool
pBoundaryEnd = (eol *> pure False) <|> (string "--" *> pure True)

-- parseLastBoundary :: ByteString -> Parser ()
-- parseLastBoundary b = endOfInput <|> (pLastBoundary b <* eol) <|> (takeLine *> parseLastBoundary b)

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

grabParts :: (MonadIO m) => Iteratee ByteString m a
                       -> Iteratee MatchInfo m [a]
grabParts partIter = do
  let iter = do
      isLast <- bParser
      if isLast
        then return Nothing
        else do
          x <- partIter
          skipToEof
          return $ Just x
  iterateeDebugWrapper "grab parts" $ go iter []
  where
    go :: (MonadIO m) => Iteratee ByteString m (Maybe a)
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

           -- output :: Maybe a 
           output <- lift $ run_ $ returnI innerStep

           case output of 
             Just x  -> go iter (x : soFar)
             Nothing -> return soFar
    
    bParser :: (MonadIO m) => Iteratee ByteString m Bool
    bParser = iterateeDebugWrapper "boundary debugger" $ iterParser $ pBoundaryEnd


internalHandleMultipart :: (MonadIO m) => ByteString -- boundary
                                     -> (ParamHeaders -> Iteratee ByteString m a)
                                     -> Iteratee ByteString m [a]
internalHandleMultipart bound clientHandler = do
  -- kmpEnumeratee bound :: Enumeratee ByteString MatchInfo m1 a1
  -- runIteratee $ grabParts clientIter :: m (Step MatchInfo m [a])
  _ <- iterParser $ parseFirstBoundary bound
  partsStep <- lift $ runIteratee $ grabParts $ compressIteratee clientHandler
  enumStep  <- iterateeDebugWrapper "kmp" $ kmpEnumeratee fullBound partsStep



  output <- lift $ run_ $ returnI enumStep
  return output

  where 
    fullBound = S.concat ["\r\n", "--", bound]

    compressIteratee :: (MonadIO m) => (ParamHeaders -> Iteratee ByteString m a) -> Iteratee ByteString m a
    compressIteratee handler = do
      headers <- iterateeDebugWrapper "header parser" $ iterParser pParamHeaders
      handler headers

  
------
testHandler :: ParamHeaders -> Iteratee ByteString IO [ByteString]
testHandler _ = consume

--
testStr :: ByteString
testStr = s2b $ "------WebKitFormBoundaryoagG1yA8o4dcYBL2\r\nContent-Disposition: form-data; name=\"input1\"\r\n\r\nhey\r\n------WebKitFormBoundaryoagG1yA8o4dcYBL2\r\nContent-Disposition: form-data; name=\"input2\"\r\n\r\nyou\r\n------WebKitFormBoundaryoagG1yA8o4dcYBL2\r\nContent-Disposition: form-data; name=\"input3\"\r\n\r\nguys\r\n------WebKitFormBoundaryoagG1yA8o4dcYBL2\r\nContent-Disposition: form-data; name=\"file1\"; filename=\"test4.txt\"\r\nContent-Type: text/plain\r\n\r\n21\r\n\r\n------WebKitFormBoundaryoagG1yA8o4dcYBL2--\r\n"

testStr2 = s2b $ "------WebKitFormBoundaryorypxRjAcffML0UV\r\nContent-Disposition: form-data; name=\"input1\"\r\n\r\nthis\r\n------WebKitFormBoundaryorypxRjAcffML0UV\r\nContent-Disposition: form-data; name=\"input2\"\r\n\r\nis\r\n------WebKitFormBoundaryorypxRjAcffML0UV\r\nContent-Disposition: form-data; name=\"input3\"\r\n\r\na\r\n------WebKitFormBoundaryorypxRjAcffML0UV\r\nContent-Disposition: form-data; name=\"file1\"; filename=\"fnce-exam-notes.org\"\r\n\r\n* 10 Qs on \n\r\n------WebKitFormBoundaryorypxRjAcffML0UV--\r\n"

testStr3 = s2b $ "------WebKitFormBoundarynQqnp4UlAoFN3BIl\r\nContent-Disposition: form-data; name=\"input1\"\r\n\r\nthis\r\n------WebKitFormBoundarynQqnp4UlAoFN3BIl\r\nContent-Disposition: form-data; name=\"input2\"\r\n\r\nis\r\n------WebKitFormBoundarynQqnp4UlAoFN3BIl\r\nContent-Disposition: form-data; name=\"input3\"\r\n\r\na\r\n------WebKitFormBoundarynQqnp4UlAoFN3BIl\r\nContent-Disposition: form-data; name=\"file1\"; filename=\"somalinotes.txt\"\r\nContent-Type: text/plain\r\n\r\nOutline:\n\nSomalia - where it is/what it is/governmental issues\nfishermen make no money\nPirates make a ton\nsocial status of pirates\nvideo(?)\nWhy pirates do it - their rationale at least\nProblem growing over last 2-3 years\nWhere the money goes\nThesis\n\n\nWhy?\n1) government supports piracy\n2) government fails to protect somali waters from fishing, illegal dumping, so people have to take it upon themselves\n3) Fisherman can't fish if the water's overfished\n4) Huge payoff, quick money\r\n------WebKitFormBoundarynQqnp4UlAoFN3BIl--\r\n"

testHand :: Iteratee ByteString IO [[ByteString]]
testHand =  internalHandleMultipart "----WebKitFormBoundaryoagG1yA8o4dcYBL2" testHandler

testHand2 :: Iteratee ByteString IO [[ByteString]]
testHand2 =  internalHandleMultipart "----WebKitFormBoundaryorypxRjAcffML0UV" testHandler

testHand3 :: Iteratee ByteString IO [[ByteString]]
testHand3 =  internalHandleMultipart "----WebKitFormBoundarynQqnp4UlAoFN3BIl" testHandler

s2b x = S.pack $ map c2w x

--testAct :: (MonadSnap m) => m [L.ByteString]
testAct = do
  step <- liftIO $ runIteratee testHand
  enumBS testStr step

--testAct :: (MonadSnap m) => m [L.ByteString]
testAct2 = do
  step <- liftIO $ runIteratee testHand2
  enumBS testStr2 step

--testAct :: (MonadSnap m) => m [L.ByteString]
testAct3 = do
  step <- liftIO $ runIteratee testHand3
  enumBS testStr3 step
  

