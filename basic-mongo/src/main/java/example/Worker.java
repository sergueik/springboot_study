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
	public Model findOneByRepo() throws IOException {
		Iterator<Model> m = mongoRepository.findAll().iterator();
		if (m.hasNext()) {
			Model obj = m.next();
			obj.setId(1);
			return obj;
		} else {
			return null;
		}
	}

	@RequestMapping(value = "/insert/{value}", method = RequestMethod.GET)
	public void saveByRepo(@PathVariable String value) {
		Model model = new Model();
		model.setId(System.currentTimeMillis());
		model.setValue(value);
		mongoRepository.save(model);
	}
}
