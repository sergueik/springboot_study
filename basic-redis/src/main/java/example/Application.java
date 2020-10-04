package example;

import redis.clients.jedis.Jedis;

import java.util.List;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@SpringBootApplication
@RestController
@RequestMapping("/basic")
public class Application {
	public static final String redisHost = System.getenv()
			.getOrDefault("REDIS_HOST", "127.0.0.1");
	public static final String redisPort = System.getenv()
			.getOrDefault("REDIS_PORT", "6379");

	public static Jedis jedis = null;

	public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
	}

	@GetMapping
	public String hello() {
		StringBuffer data = new StringBuffer();

		jedis = new Jedis(redisHost, Integer.parseInt(redisPort), 10000);
		data.append("Connection to server sucessful\n");
		data.append("Server is running: " + jedis.ping());

		// set the data in redis string
		jedis.set("tutorial-name", "Redis tutorial");
		// Get the stored data and print it
		data.append(
				"Stored string in redis:: " + jedis.get("tutorial-name") + "\n");

		jedis.lpush("tutorial-list", "Redis");
		jedis.lpush("tutorial-list", "Mongodb");
		jedis.lpush("tutorial-list", "Mysql");
		List<String> list = jedis.lrange("tutorial-list", 0, 5);

		for (int i = 0; i < list.size(); i++) {
			data.append("Stored string in redis:: " + list.get(i) + "\n");
		}

		// store data in redis list
		// Get the stored data and print it
		Object[] listkeys = jedis.keys("*").toArray();

		for (int i = 0; i < listkeys.length; i++) {
			data.append("List of stored keys:: " + listkeys[i].toString() + "\n");
		}
		return data.toString();

	}
}
