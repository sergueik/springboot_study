package example.utils;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Random;
import java.util.UUID;

public class RandomValueFactory {

	private Random rnd = new Random();

	public String randomString(int len) {
		return UUID.randomUUID().toString().substring(0, len);
	}

	public BigDecimal randomDecimal() {
		return BigDecimal.valueOf(rnd.nextDouble() * 10000).setScale(2, RoundingMode.HALF_UP);
	}

	public int randomInt(int max) {
		return rnd.nextInt(max);
	}

}
