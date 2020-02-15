package example;

import java.io.IOException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Component
@RestController
@RequestMapping("/all")
public class Basic {

	@Autowired
	private ModelMongoRepository mongoRepository;

	@Autowired
	private MongoTemplate mongoTemplate;

	@GetMapping
	public Iterable<Model> findAll() throws IOException {
		return mongoRepository.findAll();
	}
}
