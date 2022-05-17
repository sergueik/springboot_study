package example.controller;

/**
 * Copyright 2022 Serguei Kouzmine
 */

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import org.springframework.web.bind.annotation.RestController;

import example.model.Customer;
import example.projection.CustomerItem;
import example.projection.ServerInstanceApplication;
import example.repository.AxixsRepository;
import example.repository.CustomerRepository;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@RestController
public class ProductController {

	private static Logger logger = LoggerFactory
			.getLogger(ProductController.class);

	// NOTE - used implementing class instead of interface
	@Autowired
	private CustomerRepository customerRepository;

	@Autowired
	private AxixsRepository axixsRepository;

	@RequestMapping(value = "/data", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<List<Object[]>> getUntypedDataWithNulls() {
		List<Object[]> data = customerRepository.findAllUntypedDataWithNulls();
		logger.info("returned " + data.size() + " rows");
		return ResponseEntity.status(HttpStatus.OK).body(data);
	}

	@RequestMapping(value = "/customers", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Collection<Customer>> getCustomers() {
		Collection<Customer> data = customerRepository.findAllCustomers();
		logger.info("returned " + data.size() + " rows");
		return ResponseEntity.status(HttpStatus.OK).body(data);
	}

	@RequestMapping(value = "/customer/{id}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Collection<Customer>> getCustomer(
			@PathVariable(required = true) Integer id) {
		Collection<Customer> data = customerRepository.findCustomer(id);
		logger.info("returned " + data.size() + " rows");
		return ResponseEntity.status(HttpStatus.OK).body(data);
	}

	@RequestMapping(value = "/names", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Collection<String[]>> getNames() {
		Collection<String[]> data = customerRepository.findAllNames();
		logger.info("returned " + data.size() + " rows");
		return ResponseEntity.status(HttpStatus.OK).body(data);
	}

	@RequestMapping(value = "/items", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Collection<CustomerItem>> getCustomerItems() {
		Collection<CustomerItem> data = customerRepository.findAllCustomerItems();
		logger.info("returned " + data.size() + " rows");
		return ResponseEntity.status(HttpStatus.OK).body(data);
	}

	@RequestMapping(value = "/info", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Collection<ServerInstanceApplication>> findAxixes() {
		Collection<ServerInstanceApplication> data = axixsRepository.findAxixes();
		logger.info("returned " + data.size() + " rows");
		return ResponseEntity.status(HttpStatus.OK).body(data);
	}

}
