{-# LANGUAGE OverloadedStrings #-}
module Yesod.Session.Redis (
		localRedisSessionBackend,
		redisSessionBackend
	) where

import qualified Web.RedisSession as R
import Yesod.Core
import qualified Network.Wai as W
import Web.Cookie
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, addUTCTime)
import Data.Conduit.Pool (Pool)
import Data.Binary
import Data.Text (Text)
import Data.Text.Encoding
import Control.Monad (liftM)

instance Binary Text where 
	put = put . encodeUtf8 
	get = liftM decodeUtf8 get

sessionName = "yesodSession"

loadRedisSession :: (Yesod master) => Pool R.Redis -> master -> W.Request -> UTCTime -> IO BackendSession
loadRedisSession pool _ req now = do
	let val = do
		raw <- lookup "Cookie" $ W.requestHeaders req
		lookup sessionName $ parseCookies raw
	case val of
		Nothing -> return []
		Just s -> fmap (fromMaybe []) $ liftIO $ R.getSession pool s

saveRedisSession :: (Yesod master) => Pool R.Redis -> Int -> master -> W.Request -> UTCTime -> BackendSession -> BackendSession -> IO [Header]
saveRedisSession pool timeout master req now _ sess = do
	let val = do
		raw <- lookup "Cookie" $ W.requestHeaders req
		lookup sessionName $ parseCookies raw
	key <- case val of
		Nothing -> R.newKey
		Just k -> return k
	R.setSessionExpiring pool key sess timeout
	return [AddCookie def {
			setCookieName = sessionName,
			setCookieValue = key,
			setCookiePath = Just $ cookiePath master,
			setCookieExpires = Just expires,
			setCookieDomain = cookieDomain master,
			setCookieHttpOnly = True
		}]
	where
		expires = fromIntegral (timeout * 60) `addUTCTime` now


localRedisSessionBackend :: (Yesod master) => Int -> IO (SessionBackend master)
localRedisSessionBackend = sessionBackend R.makeRedisLocalConnectionPool

redisSessionBackend :: (Yesod master) => String -> String -> Int -> IO (SessionBackend master)
redisSessionBackend server port = sessionBackend (R.makeRedisConnectionPool server port)

sessionBackend :: (Yesod master) => IO (Pool R.Redis) -> Int -> IO (SessionBackend master)
sessionBackend mkPool timeout = do
	pool <- mkPool
	return $ SessionBackend {
		sbSaveSession = saveRedisSession pool timeout,
		sbLoadSession = loadRedisSession pool
	}
