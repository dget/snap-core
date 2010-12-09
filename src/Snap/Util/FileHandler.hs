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

data MultipartChunk = Boundary  Bool -- ^ True == final boundary
                    | NextChunk ByteString
                    deriving Show

getLineIncludingLF :: Parser ByteString
getLineIncludingLF = do
  s <- takeWhile (/=(c2w '\n'))
  char '\n'

  -- snoc is ugly, but nothing better
  return $ S.snoc s (c2w '\n')

getNextChunk :: ByteString -> Parser MultipartChunk
getNextChunk boundary = checkBoundary <|>
                        chunkWithoutNewlines <|>
                        chunkWithNewlines
  where
    checkBoundary = try $ Boundary <$> pBoundary boundary
    
    chunkWithoutNewlines = try $ do
      ensure bUFSIZ
      s <- A.take bUFSIZ
      if S.any (== (c2w '\n')) s
         then empty -- fail and will backtrack since there's a newline
         else return $ NextChunk s

    chunkWithNewlines = NextChunk <$> getLineIncludingLF

handlePartBody :: ByteString
               -> ParamHeaders                               -- already parsed
               -> (ParamHeaders -> Iteratee ByteString IO a) -- handler
               -> Iteratee ByteString IO (a, Bool)
handlePartBody boundary headers userHandler = do
  -- turn client iteratee into a step
  userStep <- lift $ runIteratee $ userHandler headers

  -- lift the step up to be (a, False) - the false will be replaced by loop
  -- and start the loop

  joinI $ checkDone loop $ liftStep userStep

  where 
  finish :: Monad m => Bool 
         -> Iteratee a m b
         -> Iteratee a m (b, Bool)
  finish t iter = liftM (,t) iter

  replacesnd :: Monad m => Bool
             -> Iteratee a m (b, Bool)
             -> Iteratee a m (b, Bool)
  replacesnd t iter = liftM (repl t) iter
    where repl x (a, _) = (a, x)
  
  -- takes the step, raises b to (b, Bool), sets the bool false..
  liftStep :: Monad m => Step a m b -> Step a m (b, Bool)
  liftStep (Continue k) = Continue ((finish False) . k)
  liftStep (Yield x y) = Yield (x, False) y
  liftStep (Error err) = Error err

  checkDone' :: Enumeratee ByteString ByteString IO (a, Bool)
  checkDone' (Yield x chunk) = return $ Yield x chunk
  checkDone' (Error e) = return $ Error e
    

  loop :: (Stream ByteString -> Iteratee ByteString IO (a, Bool)) 
       -> Iteratee ByteString IO (Step ByteString IO (a, Bool))
  loop k = do

    nextChunk <- iterateeDebugWrapper "next chunk parser" $ iterParser $ getNextChunk boundary


    case nextChunk of
      (Boundary lastBoundary) -> do
          replacesnd lastBoundary $ k EOF
          lift $ runIteratee $ replacesnd lastBoundary $ k EOF
      (NextChunk s) -> do
        step <- lift $ runIteratee $ k $ Chunks [s]
        checkDone loop step

handlePartBody' :: ByteString
                -> ParamHeaders                               -- already parsed
                -> (ParamHeaders -> Iteratee ByteString IO a) -- handler
                -> Iteratee ByteString IO (a, Bool)
handlePartBody' boundary headers handler = do 
    userStep <- lift $ runIteratee $ handler headers
    handleStep <- lift $ runIteratee $ iterateeDebugWrapper "part handler" $ partHandler userStep

    iterateeDebugWrapper "kmp" $ joinI' $ kmpEnumeratee trueBoundary handleStep
  where
    trueBoundary = S.append "--" boundary
  
    -- modifying joinI to also flatten any remaining MatchInfo
    joinI' :: Iteratee ByteString IO (Step MatchInfo IO b) -> Iteratee ByteString IO b
    joinI' outer = do
        step <- lift $ runIteratee $ outer
        case step of
          Error e -> throwError e
          Continue k -> joinI' $ continue k
          Yield b (Chunks xs) -> (debug $ show xs) >> check b xs
     where
       check (Continue k) xs = k EOF >>== \s -> case s of
        	Continue _ -> error "joinI: divergent iteratee"
        	_ -> check s xs
       check (Yield x (Chunks a)) xs = yield x $ Chunks $ xs ++ map m2b a
       check (Error e) _ = throwError e
--    where
--      check (Error e) = throwError e
--      check (Continue k) =  k

    -- joinI' outer = outer >>= check where
    --     check (Continue k) = k EOF >>== \s -> case s of
    --     	Continue _ -> error "joinI: divergent iteratee"
    --     	_ -> check s
    --     check (Yield x (Chunks a)) = yield x $ Chunks $ map m2b a
    --     check (Error e) = throwError e

    m2b :: MatchInfo -> ByteString
    m2b (Match a) = a
    m2b (NoMatch a) = a

    -----------------------------------------------------------
    -- This should take a user step which handles the body. 
    -- If a NoMatch is found, should take it, and either wait for more, or for a match
    -- depending on the result from the continuation.
    -- If a Match is found, should feed an EOF into the continuation, and pass the 
    -- result and Match to waitForMatch if value given, error otherwise
    -- If empty, will issue a continuation that takes a NoMatch (this)
    -----------------------------------------------------------
    contFun :: (Stream ByteString -> Iteratee ByteString IO a) -- user step continuation
            -> Stream MatchInfo 
            -> Iteratee MatchInfo IO (a, Bool)
    contFun f (Chunks []) = continue $ contFun f
    contFun f (Chunks ((NoMatch a):xs)) = do
        debug $ "NoMatch: " ++ show a
        userStep' <- lift $ runIteratee $ iterateeDebugWrapper "user handler" $ 
                     f $ Chunks [a]
        case userStep' of
          (Error e)    -> throwError e
          (Yield b _)  -> iterateeDebugWrapper "waiting for match" $ waitForMatch b $ Chunks xs
          (Continue k) -> contFun k $ Chunks xs
    contFun f c@(Chunks ((Match a): xs)) = do
        debug $ "match: " ++ show a
        userStep' <- lift $ runIteratee $ f $ EOF
        case userStep' of
          (Error e)    -> throwError e
          (Yield b _)  -> iterateeDebugWrapper "waiting for match" $ waitForMatch b c
          (Continue k) -> error "divergent iteratee"
    contFun f EOF = error "not enough input"

    -- discards NoMatch until it finds a match, then yields the values.
    waitForMatch :: a -> Stream MatchInfo -> Iteratee MatchInfo IO (a, Bool)
    waitForMatch ret (Chunks []) = continue $ waitForMatch ret
    waitForMatch ret (Chunks ((NoMatch a):xs)) = waitForMatch ret $ Chunks xs
    waitForMatch ret (Chunks ((Match a):xs))   = do
        parseStep <- lift $ runIteratee $ iterParser $ (pBoundary boundary)
        -- the actual boundary
        pStep  <- lift $ runIteratee $ case parseStep of 
          (Error e)    -> throwError e
          (Yield b _)  -> error "unreachable case"
          (Continue k) -> k $ Chunks [a]

        case pStep of
          (Error e)    -> throwError e
          (Continue k) -> boundaryContinue ret k $ Chunks xs -- the eol or "--"
          (Yield b _) -> error "unreachable case"
    waitForMatch ret EOF = error "not enough input"

    -- should keep feeding data from NoMatch until either errors or yield 
    boundaryContinue :: a  -- the value to return
                     -> (Stream ByteString -> Iteratee ByteString IO Bool)
                                                    -- parser continuation   
                     -> Stream MatchInfo -- the stream
                     -> Iteratee MatchInfo IO (a, Bool)
    boundaryContinue ret p (Chunks []) = continue $ boundaryContinue ret p 
    boundaryContinue ret p (Chunks ((NoMatch a):xs)) = do
      pStep <- lift $ runIteratee $ p $ Chunks [a]
      case pStep of
        (Continue k)           -> boundaryContinue ret k $ Chunks xs 
        (Yield b (Chunks xs')) -> yield (ret, b) $ Chunks (map NoMatch xs' ++ xs)
        (Error e)              -> throwError e

    partHandler :: Step ByteString IO a -> Iteratee MatchInfo IO (a, Bool) 
    partHandler (Yield b _) = return $ (b, False)
    partHandler (Error e)   = throwError e
    partHandler (Continue k) = continue $ contFun k
    

internalHandleMultipart :: ByteString
                        -> (ParamHeaders -> Iteratee ByteString IO a)
                        -> Iteratee ByteString IO [a]
internalHandleMultipart boundary userHandler = do
  finished <- iterateeDebugWrapper "first boundary" $ iterParser $ parseFirstBoundary boundary
  if finished
    then return []
    else go []

  where
    go l = do
      headers <- iterateeDebugWrapper "headers" $ iterParser pParamHeaders
      debug $ show $ name headers

      (x, finished) <- handlePartBody' boundary headers userHandler

      let l' = x:l

      if finished
        then return l'
        else go l'


------
testHandler :: ParamHeaders -> Iteratee ByteString IO [ByteString]
testHandler h = do
  case fileInfo h of 
    Nothing ->  do
      consume
    Just _  -> do
      consume

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
  