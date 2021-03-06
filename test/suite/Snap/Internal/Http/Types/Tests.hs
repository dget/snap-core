{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Snap.Internal.Http.Types.Tests
  ( tests ) where

import           Control.Monad
import           Control.Parallel.Strategies
import qualified Data.ByteString.Char8 as S
import           Data.ByteString.Char8 (ByteString)
import           Data.ByteString.Lazy.Char8 ()
import           Data.IORef
import           Data.List (sort)
import qualified Data.Map as Map
import           Data.Time.Calendar
import           Data.Time.Clock
import           Prelude hiding (take)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test, path)
import           Text.Regex.Posix

import           Snap.Internal.Http.Types
import           Snap.Iteratee


tests :: [Test]
tests = [ testTypes
        , testUrlDecode
        , testFormatLogTime
        , testAddHeader ]


mkRq :: IO Request
mkRq = do
    enum <- newIORef (SomeEnumerator $ enumBS "")
    return $ Request "foo" 80 "foo" 999 "foo" 1000 "foo" False Map.empty
                 enum Nothing GET (1,1) [] "" "/" "/" "/" "" Map.empty


testFormatLogTime :: Test
testFormatLogTime = testCase "formatLogTime" $ do
    b <- formatLogTime 3804938

    let re = ("^[0-9]{1,2}/[A-Za-z]{3}/[0-9]{4}:[0-9]{2}:[0-9]{2}:[0-9]{2} (-|\\+)[0-9]{4}$"
                  :: ByteString)

    assertBool "formatLogTime" $ b =~ re


testAddHeader :: Test
testAddHeader = testCase "addHeader" $ do
    defReq <- mkRq

    let req = addHeader "foo" "bar" $
              addHeader "foo" "baz" defReq


    let x = getHeader "foo" req
    assertEqual "addHeader x 2" (Just "bar baz") x


testUrlDecode :: Test
testUrlDecode = testCase "urlDecode" $ do
    assertEqual "bad hex" Nothing $ urlDecode "%qq"


testTypes :: Test
testTypes = testCase "show" $ do
    defReq <- mkRq

    let req = rqModifyParams (Map.insert "zzz" ["bbb"]) $
              updateHeaders (Map.insert "zzz" ["bbb"]) $
              rqSetParam "foo" ["bar"] $
              defReq

    let req2 = (addHeader "zomg" "1234" req) { rqCookies = [ cook, cook2 ] }

    let !a = show req `using` rdeepseq
    let !_ = show req2 `using` rdeepseq

    -- we don't care about the show instance really, we're just trying to shut
    -- up hpc
    assertBool "show" $ a /= b
    assertEqual "rqParam" (Just ["bar"]) (rqParam "foo" req)
    assertEqual "lookup" (Just ["bbb"]) (Map.lookup "zzz" $ rqParams req)
    assertEqual "lookup 2" (Just ["bbb"]) (Map.lookup "zzz" $ headers req)
    assertEqual "cookie" (Just ["foo=bar; path=/; expires=Sat, 30-Jan-2010 00:00:00 GMT; domain=.foo.com"]) cookieHeader

    assertEqual "cookie2" (Just ["foo=bar; path=/; expires=Sat, 30-Jan-2010 00:00:00 GMT; domain=.foo.com", "foo=baz; path=/; expires=Sat, 30-Jan-2010 00:00:00 GMT; domain=.foo.com"]) (liftM sort cookieHeader2)

    assertEqual "cookie3" (Just ["foo=baz"]) cookieHeader3

    assertEqual "response status" 555 $ rspStatus resp
    assertEqual "response status reason" "bogus" $ rspStatusReason resp
    assertEqual "content-length" (Just 4) $ rspContentLength resp
    -- run response body
    let benum = rspBodyToEnum $ rspBody resp
    bd <- runIteratee consume >>= run_ . benum
    assertEqual "response body" "PING" $ S.concat bd

    let !_ = show GET
    let !_ = GET == POST
    let !_ = headers $ headers defReq

    let !_ = show resp2 `using` rdeepseq


    return ()

  where
    resp = addCookie cook $
           setContentLength 4 $
           modifyResponseBody id $
           setResponseBody (enumBS "PING") $
           setContentType "text/plain" $
           setResponseStatus 555 "bogus" $
           emptyResponse
    !b = show resp `using` rdeepseq

    resp2 = addCookie cook2 resp
    resp3 = addCookie cook3 emptyResponse


    utc   = UTCTime (ModifiedJulianDay 55226) 0
    cook  = Cookie "foo" "bar" (Just utc) (Just ".foo.com") (Just "/")
    cook2 = Cookie "foo" "baz" (Just utc) (Just ".foo.com") (Just "/")
    cook3 = Cookie "foo" "baz" Nothing Nothing Nothing

    cookieHeader = Map.lookup "Set-Cookie" $ headers resp
    cookieHeader2 = Map.lookup "Set-Cookie" $ headers resp2
    cookieHeader3 = Map.lookup "Set-Cookie" $ headers resp3

