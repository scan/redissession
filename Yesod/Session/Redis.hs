{-# LANGUAGE OverloadedStrings #-}
module Yesod.Session.Redis (
		localRedisSessionBackend,
		redisSessionBackend
	) where

import qualified Web.RedisSession as R
import Yesod.Core
import Yesod.Internal
import qualified Network.Wai as W
import Web.Cookie
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)
import Data.Conduit.Pool (Pool)

--loadRedisSession :: (Yesod master) => Pool R.Redis -> master -> W.Request -> UTCTime -> IO Session
loadRedisSession pool _ req now = do
	let val = do
		raw <- lookup "Cookie" $ W.requestHeaders req
		lookup sessionName $ parseCookies raw
	case val of
		Nothing -> return []
		Just s -> fmap (fromMaybe []) $ liftIO $ R.getSession pool s

saveRedisSession pool timeout master req now _ sess = do
	let val = do
		raw <- lookup "Cookie" $ W.requestHeaders req
		lookup sessionName $ parseCookies raw
	key <- case val of
		Nothing -> R.newKey
		Just k -> return k
	R.setSessionExpiring pool key expires sess
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

localRedisSessionBackend = sessionBackend makeRedisLocalConnectionPool

redisSessionBackend = sessionBackend makeRedisConnectionPool

sessionBackend mkPool timeout = do
	pool <- mkPool
	SessionBackend {
		sbSaveSession :: saveRedisSession pool timeout,
		sbLoadSession :: loadRedisSession pool
	}
