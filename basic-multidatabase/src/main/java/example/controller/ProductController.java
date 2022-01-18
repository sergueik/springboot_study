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

import example.model.InputRequest;
import example.model.MongoProduct;
import example.model.MySqlProduct;
import example.model.Product;
import example.repository.ProductMongoRepository;
import example.repository.ProductMySqlRepository;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@RestController
public class ProductController {

	private static Logger logger = LoggerFactory
			.getLogger(ProductController.class);

	@Autowired
	private ProductMongoRepository mongoRepository;
	@Autowired
	private ProductMySqlRepository mySqlRepository;

	@PostMapping(value = "/products", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<String> postProducts(
			@RequestBody InputRequest request) {

		mongoRepository.save(new MongoProduct(request.getId(), request.getName(),
				request.getQty(), request.getPrice()));
		logger.info("product added on Mongo db");
		mySqlRepository.save(new MySqlProduct(request.getId(), request.getName(),
				request.getQty(), request.getPrice()));
		logger.info("product added on MySQL db");
		return ResponseEntity.ok().build();
	}

	// https://stackoverflow.com/questions/29612083/casting-a-list-of-an-object-to-a-list-of-super-types/29612111
	@RequestMapping(value = "/products", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)

	public ResponseEntity<List<? extends Product>> getProducts(
			@RequestParam(value = "db", required = false, defaultValue = "mysql") String db) {
		// avoid explicit List of supertype conversion
		// mongoRepository.findAll().stream().map(o -> new Product(o.getId(), o.getName(), o.getQty(), o.getPrice())).collect(Collectors.toList());

		if (db.equalsIgnoreCase("mongo")) {
			List<MongoProduct> data = mongoRepository.findAll();
			logger.info("found: {} rows in Mongo DB", data.size());
			return ResponseEntity.status(HttpStatus.OK).body(data);

		} else if (db.equalsIgnoreCase("mysql")) {
			List<MySqlProduct> data = mySqlRepository.findAll();
			logger.info("found: {} rows in MySQL DB", data.size());
			return ResponseEntity.status(HttpStatus.OK).body(data);
		} else {
			System.err.println("invalid operation: " + db);
			return ResponseEntity.status(HttpStatus.METHOD_NOT_ALLOWED)
					.body(new ArrayList<Product>());
		}
	}

}
