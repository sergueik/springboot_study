package example.controller;

/**
 * Copyright 2022 Serguei Kouzmine
 */

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import org.springframework.web.bind.annotation.RestController;

import example.model.CustomerItem;
import example.model.InputRequest;
import example.model.MySqlProduct;
import example.model.Product;
import example.repository.CustomerRepository;
import example.repository.CustomerRepositoryDao;
import example.repository.ProductMySqlRepository;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@RestController
public class ProductController {

	private static Logger logger = LoggerFactory
			.getLogger(ProductController.class);

	// NOTE - use implemented class not interface
	// NOTE:
	// field customerRepository in example.controller.ProductController required a
	// bean
	// of type 'example.repository.CustomerRepositoryDao' that could not be found.
	@Autowired
	private CustomerRepositoryDao customerRepository;

	@Autowired
	private ProductMySqlRepository mySqlRepository;

	@PostMapping(value = "/products", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<String> postProducts(
			@RequestBody InputRequest request) {
		logger.info("Processing post request intp products with:" + " id: "
				+ request.getId() + " name: " + request.getName() + " qty: "
				+ request.getQty() + " price: " + request.getPrice());

		mySqlRepository.save(new MySqlProduct(request.getId(), request.getName(),
				request.getQty(), request.getPrice()));
		logger.info("product added on MySQL db");
		return ResponseEntity.ok().build();
	}

	// https://stackoverflow.com/questions/29612083/casting-a-list-of-an-object-to-a-list-of-super-types/29612111
	@RequestMapping(value = "/cust", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<List<CustomerItem>> getCustomers() {
		List<CustomerItem> data = new ArrayList<>();
		data.add(new CustomerItem());
		logger.info("calling findCustomerDetailsByCustomerId");
		customerRepository.findCustomerDetailsByCustomerId(0);
		return ResponseEntity.status(HttpStatus.OK).body(data);
	}

	// https://stackoverflow.com/questions/29612083/casting-a-list-of-an-object-to-a-list-of-super-types/29612111
	@RequestMapping(value = "/products", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<List<? extends Product>> getProducts(
			@RequestParam(value = "db", required = false, defaultValue = "mysql") String db) {
		List<MySqlProduct> data = mySqlRepository.findAll();
		logger.info("found: {} rows in MySQL DB", data.size());
		return ResponseEntity.status(HttpStatus.OK).body(data);
	}

}
