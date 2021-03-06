package example;

import redis.clients.jedis.Jedis;

import java.util.List;
import java.util.stream.Collectors;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@SpringBootApplication
@RestController
@RequestMapping("/redis")
public class Application {
	public static final String host = System.getenv().getOrDefault("REDIS_HOST", "127.0.0.1");
	public static final String port = System.getenv().getOrDefault("REDIS_PORT", "6379");

	public static Jedis jedis = null;

	public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
	}

	@GetMapping
	public String hello() {
		StringBuffer data = new StringBuffer();

		jedis = new Jedis(host, Integer.parseInt(port), 10000);
		data.append(String.format("Connection to Redis server %s %s sucessful\n", host, port));
		data.append("Server is running: " + jedis.ping() + "\n");

		jedis.set("title", "Redis tutorial");
		data.append("Stored string: " + jedis.get("title") + "\n");

		jedis.lpush("numbers", Double.valueOf(1).toString());
		jedis.lpush("numbers", Double.valueOf(2).toString());
		jedis.lpush("numbers", Double.valueOf(3).toString());

		List<Double> numbers = jedis.lrange("numbers", 0, 10).stream().map(o -> Double.valueOf(o))
				.collect(Collectors.toList());
		data.append("Stored range of numbers: "
				+ String.join(",", numbers.stream().map(Object::toString).collect(Collectors.toList())) + "\n");

		Object[] keys = jedis.keys("*").toArray();
		data.append("List of stored keys: ");
		for (int cnt = 0; cnt < keys.length; cnt++) {
			data.append(keys[cnt].toString() + ((cnt == keys.length - 1) ? "" : ","));
		}
		data.append("\n");
		return data.toString();

	}
}
