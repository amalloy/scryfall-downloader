{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main (main) where

import Prelude hiding (putStr)

import Control.Concurrent (threadDelay)

import Data.List (intercalate)
import Data.ByteString.Char8 (ByteString, putStr, pack, unpack)

-- For case-insensitive header names
import           Data.CaseInsensitive  ( CI )

import Network.HTTP.Client.TLS
import Network.HTTP.Simple
import Network.HTTP.Types.Status (Status(..))
import Network.HTTP.Client.Conduit (Manager)

import System.Environment (getArgs)
import System.Exit (die)

-- Scryfall's rate limit for card searches is 2 per second.
-- We'll conservatively wait a whole second, since there's no rush.
rateLimitSeconds :: Int
rateLimitSeconds = 1

-- Setting a user agent to be a good citizen, and because Scryfall asks us to.
userAgent :: ByteString
userAgent = "amalloy's personal Scryfall CSV downloader"

-- Scryfall says you must set one of these, and lists */* as an option.
-- Probably http-conduit would set something reasonable, but let's be sure.
acceptHeader :: ByteString
acceptHeader = "*/*"

-- Search returns a paginated list, with hasMore and nextPage headers
nextPageHeaderName :: CI ByteString
nextPageHeaderName = "X-Scryfall-Next-Page"

setupRequest :: Request -> Request
setupRequest = addRequestHeader "Accept" acceptHeader
             . addRequestHeader "User-Agent" userAgent
             . setRequestMethod "GET"
             . setRequestSecure True
             . setRequestHost "api.scryfall.com"
             . setRequestPort 443

paginate :: Manager -> Request -> IO ByteString
paginate manager = go
  where go req = do
          let req' = setupRequest
                   . setRequestManager manager
                   $ req
          resp <- httpBS req'
          case getResponseStatus resp of
            Status 200 _ -> do
              let body = getResponseBody resp
              case getResponseHeader nextPageHeaderName resp of
                [uri] -> do
                  nextPage <- parseRequest (unpack uri)
                  threadDelay $ 1000000 * rateLimitSeconds
                  (body <>) <$> go nextPage
                _ -> pure body
            _ -> error (show resp)

main :: IO ()
main = do
  query <- getArgs >>= \case
    [] -> die "no query string provided"
    args -> pure . pack $ intercalate " " args
  manager <- newTlsManagerWith tlsManagerSettings
  let req = setRequestQueryString [ ("format", Just "csv")
                                  , ("order", Just "color")
                                  , ("q", Just query)
                                  ]
          . setRequestPath "/cards/search"
          $ defaultRequest
  body <- paginate manager req
  putStr body
