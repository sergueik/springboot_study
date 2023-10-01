package example.controller;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import example.model.Customer;
import example.utils.Utils;

@Controller
public class RestController {

	private Utils utils = Utils.getInstance();
	private Logger logger = LoggerFactory.getLogger(this.getClass());

	@RequestMapping(value = "/creditscore", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
	@ResponseBody
	public ResponseEntity<Customer> creditScore(@RequestBody Customer customer) {
		int score = utils.setScore(customer);
		logger.info("Processing {}", customer);
		customer.setScore(score);
		ResponseEntity<Customer> response = new ResponseEntity<Customer>(customer,
				HttpStatus.OK);

		return response;
	}
}
