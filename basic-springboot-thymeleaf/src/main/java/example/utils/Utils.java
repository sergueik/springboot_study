package example.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import example.model.Customer;

public class Utils {
	
	private static Utils instance = new Utils();
	private Logger logger = LoggerFactory.getLogger(this.getClass());
	private Utils() {
	}

	public static Utils getInstance() {
		return instance;
	}

	private static final int SCORE_MAX = 800;
	private static final int SCORE_MIN = 550;

	public int setScore(Customer customer) {

		int score = Math.abs(customer.getFirstName().hashCode()
				+ customer.getLastName().hashCode() + customer.getBirthDate().hashCode()
				+ customer.getSsn().hashCode());

		logger.info("{} hashcode: {}", customer, score);

		score = score % SCORE_MAX;

		while (score < SCORE_MIN) {
			score = score + 100;
		}
		return score;
	}

}
