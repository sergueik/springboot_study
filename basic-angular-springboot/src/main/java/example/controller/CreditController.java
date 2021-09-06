package example.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import example.model.Customer;

@Controller
public class CreditController {

	private Logger logger = LoggerFactory.getLogger(this.getClass());

	private static final int SCORE_MAX = 800;
	private static final int SCORE_MIN = 550;

	@RequestMapping(value = "/creditscore", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
	@ResponseBody
	public ResponseEntity<Customer> creditScore(@RequestBody Customer customer) {

		int score = Math.abs(customer.getFirstName().hashCode() + customer.getLastName().hashCode()
				+ customer.getBirthDate().hashCode() + customer.getSsn().hashCode());

		logger.info(customer.toString() + " hashcode: " + score);

		score = score % SCORE_MAX;

		while (score < SCORE_MIN) {
			score = score + 100;
		}

		customer.setScore(score);

		ResponseEntity<Customer> response = new ResponseEntity<Customer>(customer, HttpStatus.OK);

		return response;
	}

}
