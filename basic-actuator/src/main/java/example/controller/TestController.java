package example.controller;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import example.config.PrometheusCustomMonitor;

import javax.annotation.Resource;
import java.util.Random;
import java.util.concurrent.TimeUnit;

@RestController
public class TestController {

	@Resource
	private PrometheusCustomMonitor monitor;

	@GetMapping("/hello")
	public String hello() {
		return "hello, world";
	}

	@GetMapping("/slow")
	public String slow() throws InterruptedException {
		Random random = new Random();
		int delay = random.nextInt(10);
		TimeUnit.SECONDS.sleep(delay);
		return "slow operation execution time: " + delay;
	}

	@RequestMapping("/status")
	public String order(@RequestParam(defaultValue = "0") String flag)
			throws Exception {
		monitor.getOrderCount().increment();
		if ("1".equals(flag)) {
			throw new Exception("Something went wrong!");
		}
		Random random = new Random();
		int amount = random.nextInt(100);
		monitor.getAmountSum().record(amount);
		return "Random amount: " + amount;
	}
}
