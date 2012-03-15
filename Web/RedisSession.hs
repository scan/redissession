module Web.RedisSession (
		withRedis, withRedisLocal,
		makeRedisConnectionPool,
		setSession, getSession
	) where

import Database.Redis.Redis
import Data.ByteString (ByteString)
import Data.Binary (decode, encode, Binary)
import Data.Conduit.Pool

withRedis :: String -> String -> (Redis -> IO a) -> IO a
withRedis server port f = do
	redis <- connect server port
	x <- f redis
	disconnect redis
	return x

withRedisLocal :: (Redis -> IO a) -> IO a
withRedisLocal = withRedis localhost defaultPort

makeRedisConnectionPool server port = createPool (connect server port) disconnect 1 0.5 1

setSession :: (Binary a) => ByteString -> a -> Redis -> IO ()
setSession key value redis = do
	set redis key $ encode value
	return ()

setSessionExpiring :: (Binary a) => ByteString -> a -> Int -> Redis -> IO ()
setSessionExpiring key value timeout redis = do
	setEx redis key timeout $ encode value
	return ()

getSession :: (Binary a) => ByteString -> Redis -> IO (Maybe a)
getSession key redis = do
	val <- get redis key
	case val of
		RBulk Nothing -> return Nothing
		RBulk (Just v) -> return $ Just $ decode v
		_ -> return Nothing
