module Web.RedisSession (
		makeRedisConnectionPool,
		makeRedisLocalConnectionPool,
		setSession, setSessionExpiring,
		getSession,
		newKey,
		Redis
	) where

import Database.Redis.Redis
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (toChunks)
import Data.Binary (decode, encode, Binary)
import Data.Conduit.Pool
import System.Entropy (getEntropy)
import Data.Digest.Pure.MD5 (md5)
import Data.Time (getCurrentTime)

makeRedisConnectionPool :: String -> String -> IO (Pool Redis)
makeRedisConnectionPool server port = createPool (connect server port) disconnect 1 0.5 1

makeRedisLocalConnectionPool :: IO (Pool Redis)
makeRedisLocalConnectionPool = makeRedisConnectionPool localhost defaultPort

setSession :: (Binary a) => Pool Redis -> ByteString -> a -> IO ()
setSession pool key value = withResource pool $ \redis -> do
	set redis key $ encode value
	return ()

setSessionExpiring :: (Binary a) => Pool Redis -> ByteString -> a -> Int -> IO ()
setSessionExpiring pool key value timeout = withResource pool $ \redis -> do
	setEx redis key timeout $ encode value
	return ()

getSession :: (Binary a) => Pool Redis -> ByteString -> IO (Maybe a)
getSession pool key = withResource pool $ \redis -> do
	val <- get redis key
	case val of
		RBulk Nothing -> return Nothing
		RBulk (Just v) -> return $ Just $ decode v
		_ -> return Nothing

newKey :: IO ByteString
newKey = do
	ent <- getEntropy 80
	now <- getCurrentTime
	return $ B.concat [(B.concat . toChunks . encode . md5 . encode . show) now, ent]
