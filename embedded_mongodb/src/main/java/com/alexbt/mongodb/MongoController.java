package com.alexbt.mongodb;

import java.io.IOException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/mongo")
public class MongoController {

	@Autowired
	private ModelMongoRepository mongoRepository;
	
	@Autowired
	private MongoTemplate mongoTemplate;

	@RequestMapping(path="/repo", method = RequestMethod.GET)
	public Iterable<Model> findByRepo() throws IOException {
		return mongoRepository.findAll();
	}
	
	@RequestMapping(path="/template", method = RequestMethod.GET)
	public Iterable<Model> findByTemplate() throws IOException {
		return mongoTemplate.findAll(Model.class);
	}

	@RequestMapping(value = "/repo/{value}", method = RequestMethod.GET)
	public void saveByRepo(@PathVariable String value) {
		Model model = new Model();
		model.setId(System.currentTimeMillis());
		model.setValue(value);
		mongoRepository.save(model);
	}
	
	@RequestMapping(value = "/template/{value}", method = RequestMethod.GET)
	public void saveByTemplate(@PathVariable String value) {
		Model model = new Model();
		model.setId(System.currentTimeMillis());
		model.setValue(value);
		mongoTemplate.save(model);
	}
}
