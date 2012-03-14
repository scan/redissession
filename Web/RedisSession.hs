module Web.RedisSession where

import Database.Redis.Redis

withRedis server port f = do
	redis <- connect server port
	f redis
	disconnect redis
