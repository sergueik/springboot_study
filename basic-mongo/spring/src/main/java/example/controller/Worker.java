package example.controller;

/**	
 * Copyright 2021, 2022, 2023 Serguei Kouzmine
 */

import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import example.repository.ModelMongoRepository;
import example.model.Model;

@Component
@RestController
@RequestMapping("/mongo")
public class Worker {

	@Autowired
	private ModelMongoRepository mongoRepository;

	@Autowired
	private MongoTemplate mongoTemplate;

	@RequestMapping(path = "/all", method = RequestMethod.GET)
	public Iterable<Model> findAllByRepo() throws IOException {
		// MongoRepository builds query at runtime
		return mongoRepository.findAll();
	}

	@RequestMapping(path = "/any", method = RequestMethod.GET)
	public Model findAnyByRepo() throws IOException {
		Iterator<Model> m = mongoRepository.findAll().iterator();
		return (m.hasNext()) ? m.next() : null;
		// NOTE: will always pick first
	}

	@RequestMapping(path = "/get/template/{id}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Model> findByIdTemplate(@PathVariable long id)
			throws IOException {
		Model result = mongoTemplate.findById(id, Model.class);
		if (result != null) {
			System.err.println(String.format("Result: \"%s\"", result));
			return ResponseEntity.status(HttpStatus.OK).body(result);
		} else {
			System.err.println("Nothing found.");
			return ResponseEntity.status(HttpStatus.NOT_FOUND).body(null);
		}
	}

	@RequestMapping(path = "/get/{id}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Model> findOneByRepo(@PathVariable long id)
			throws IOException {
		// Retrieves entity by its id.
		// https://docs.spring.io/autorepo/docs/spring-data-commons/1.5.1.RELEASE/api/org/springframework/data/repository/CrudRepository.html
		try {
			System.err.println(String.format("Searching: \"%d\"", id));

			// https://stackoverflow.com/questions/44101061/missing-crudrepositoryfindone-method
			Model result = mongoRepository.findById(id).orElse(null);
			if (result != null) {
				System.err.println(String.format("Result: \"%s\"", result));
				return ResponseEntity.status(HttpStatus.OK).body(result);
			} else {
				System.err.println("Nothing found.");
				return ResponseEntity.status(HttpStatus.NOT_FOUND).body(null);
			}
		} catch (IllegalArgumentException e) {
			System.err.println("Exception : " + e.getMessage());
			return ResponseEntity.status(HttpStatus.NOT_FOUND).body(null);
		}
	}

	@RequestMapping(value = "/insert2/{value}", method = RequestMethod.POST)
	public void save2ByRepo(@PathVariable String value) {
		Model model = new Model();
		model.setId(System.currentTimeMillis());
		model.setValue(value);
		mongoRepository.save(model);
	}

	@RequestMapping(value = "/insert1/{value}", method = RequestMethod.POST)
	public void save1ByRepo(@PathVariable String value) {
		Model model = new Model(System.currentTimeMillis(), value);
		mongoRepository.save(model);
	}

	@RequestMapping(value = "/find/{value}", method = RequestMethod.GET)
	public ResponseEntity<List<Model>> findByCriteria(
			@PathVariable String value) {
		Query query = new Query();
		query.addCriteria(Criteria.where("value").is(value));
		List<Model> result = mongoTemplate.find(query, Model.class);
		System.err.println(String.format("Result: \"%s\"", result));
		return ResponseEntity.status(HttpStatus.OK).body(result);
	}
}

