module Web.RedisSession where

import Database.Redis.Redis
import Data.ByteString (ByteString)
import Data.Binary (decode, encode, Binary)

withRedis server port f = do
	redis <- connect server port
	f redis
	disconnect redis

withRedisLocal f = do
	redis <- connect localhost defaultPort
	f redis
	disconnect redis

setSession :: (Binary a) => ByteString -> a -> Redis -> IO ()
setSession key value redis = do
	set redis key $ encode value
	return ()

getSession :: (Binary a) => ByteString -> Redis -> IO (Maybe a)
getSession key redis = do
	val <- get redis key
	case val of
		RBulk Nothing -> return Nothing
		RBulk (Just v) -> return $ Just $ decode v
		_ -> return Nothing
