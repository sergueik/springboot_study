package example;

import java.util.Date;
import redis.clients.jedis.Jedis;

public class Util {


	private String result = null;
	private Jedis jedis = null;
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

	public Util() {
		String host = System.getenv().getOrDefault("REDIS_HOST", "localhost");
		String port = System.getenv().getOrDefault("REDIS_PORT", "6379");
		jedis = new Jedis(host, Integer.parseInt(port), 10000);
	}

	public Util(Jedis jedis) {
		this.jedis = jedis;
	}

	public String stampData(KeyVal data) {
		try {
			result = jedis.set(data.key, data.val + "[" + new Date() + "]");
		} catch (Exception e) {
			result = "error: " + e.getMessage();
			// } finally {
			// jedis.close();
		}
		return String.format("Redis SET result: %s. Jedis object hash code: %s ", result, jedis.hashCode());
	}
}
