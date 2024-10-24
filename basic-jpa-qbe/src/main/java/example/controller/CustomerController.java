package example.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import example.model.Customers;
import example.service.CustomerService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import java.util.Optional;

@RestController
@RequestMapping("/customer")

public class CustomerController {

	private static Logger logger = LoggerFactory
			.getLogger(CustomerController.class);

	@Autowired
	private CustomerService customerService;

	@GetMapping("/list")
	public ResponseEntity<List<Customers>> getAllStates() {
		logger.info("Request received to get all available customers");
		return ResponseEntity.ok(customerService.getAll());
	}

	@GetMapping("/sorted")
	public ResponseEntity<List<Customers>> findAllSorted(
			@RequestParam(name = "auto") Optional<Boolean> auto) {
		logger.info(
				"Request received to get all available customers sorted in descending order"
						+ ((auto.isPresent() && auto.get()) ? " by JPA" : ""));
		return ResponseEntity.ok(
				(auto.isPresent() && auto.get()) ? customerService.findAllAutoSorted()
						: customerService.findAllCustomSorted());
	}

	@GetMapping("/firstname")
	public ResponseEntity<List<Customers>> getAllCustomersFirstNameEndsWith(
			@RequestParam(name = "endsWith") Optional<String> endsWith,
			@RequestParam(name = "startsWith") Optional<String> startsWith) {
		if (endsWith.isPresent()) {
			logger.info(
					"Request received to get all customers first name ends with {}",
					endsWith.get());
			return ResponseEntity.status(HttpStatus.OK)
					.body(customerService.findByFirstNameEnding(endsWith.get()));
		} else {
			if (startsWith.isPresent()) {
				logger.info(
						"Request received to get all customers first name starts with {}",
						startsWith.get());

				return ResponseEntity.status(HttpStatus.OK)
						.body(customerService.findByFirstNameStarting(startsWith.get()));
			} else {
				return ResponseEntity.status(HttpStatus.NOT_FOUND).body(null);
			}
		}
	}

	@GetMapping("/lastname")
	public ResponseEntity<List<Customers>> getAllCustomersLastNameEndsWith(
			@RequestParam(name = "endsWith") String endsWith) {
		logger.info("Request received to get all  customers last name endswith {}",
				endsWith);
		return ResponseEntity.ok(customerService.findByLastNameEnding(endsWith));
	}

	@GetMapping("/balance")
	public ResponseEntity<List<Customers>> getAllCustomersLastNameEndsWith(
			@RequestParam(name = "balance") Long balance) {
		logger.info(
				"Request received to get all  customers whose wallet balance is {}",
				balance);
		return ResponseEntity
				.ok(customerService.findByWalletBalanceEquals(balance));
	}

	@GetMapping(value = "/query", produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<List<Object[]>> queryCustomerByName(
			@RequestParam("firstName") Optional<String> firstName) {
		if (firstName.isPresent()) {
			List<Object[]> results = customerService.queryCustomers(firstName.get());
			logger.info("Request returned {} customers", results.size());
			return ResponseEntity.status(HttpStatus.OK).body(results);
		} else {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
	}

	@GetMapping("/{firstName}")
	public ResponseEntity<List<Customers>> getCustomerByName(
			@PathVariable String firstName) {
		logger.info("Request received to get all  customer by name {}", firstName);
		return ResponseEntity.ok(customerService.findByFirstName(firstName));
	}
}
