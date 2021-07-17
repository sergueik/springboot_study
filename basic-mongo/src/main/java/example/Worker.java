package example;

import java.io.IOException;
import java.util.Iterator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

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
		return mongoRepository.findAll();
	}

	@RequestMapping(path = "/any", method = RequestMethod.GET)
	public Model findAnyByRepo() throws IOException {
		Iterator<Model> m = mongoRepository.findAll().iterator();
		return (m.hasNext()) ? m.next() : null;
		// NOTE: will always pick first
	}

	@RequestMapping(path = "/get/{value}", method = RequestMethod.GET)
	public Model findOneByRepo(@PathVariable String value) throws IOException {
		// Retrieves entity by its id.
		// https://docs.spring.io/autorepo/docs/spring-data-commons/1.5.1.RELEASE/api/org/springframework/data/repository/CrudRepository.html
		try {
			long id = Long.parseLong(value);
			System.err.println(String.format("Find:\"%s\"", value));
			// NOTE: finds nothing, though querying mongodb node direct succeeds
			// use mydb
			// db.model.find({"_id":1583701210532});
			return mongoRepository.findOne(value);

		} catch (IllegalArgumentException e) {
			System.err.println("Exception : " + e.getMessage());
			return null;
		}
	}

	@RequestMapping(value = "/insert2/{value}", method = RequestMethod.GET)
	public void save2ByRepo(@PathVariable String value) {
		Model model = new Model();
		model.setId(System.currentTimeMillis());
		model.setValue(value);
		mongoRepository.save(model);
	}

	@RequestMapping(value = "/insert1/{value}", method = RequestMethod.GET)
	public void save1ByRepo(@PathVariable String value) {
		Model model = new Model(System.currentTimeMillis(), value);
		mongoRepository.save(model);
	}
}
