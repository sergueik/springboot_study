package example;

import java.util.Date;
import redis.clients.jedis.Jedis;

public class Util {

	public static class KeyVal {

		public String key;
		public String val;

		public KeyVal(String key, String val) {
			this.key = key;
			this.val = val;
		}

		public KeyVal() {
		}

	}

	Jedis jedis = null;

	public Util() {
		String redisHost = System.getenv().getOrDefault("REDIS_HOST", "localhost");
		String redisPort = System.getenv().getOrDefault("REDIS_PORT", "6379");
		jedis = new Jedis(redisHost, Integer.parseInt(redisPort), 10000);
	}

	public String set(KeyVal keyVal) {
		String result = null;
		try {
			result = jedis.set(keyVal.key, keyVal.val + "[" + new Date() + "]");
		} catch (Exception e) {
			result = "error - " + e.getMessage();
		} finally {
			jedis.close();
		}
		return "Redis SET result - " + result + ". Jedis object hash code - "
				+ jedis.hashCode();
	}
}
