package net.arunoday.metric.store.repository;

import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.StringUtils;

/**
 * Repository test helper.
 * 
 * @author Aparna Chaudhary
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = { "/testApplicationContext.xml" })
public abstract class AbstractRepositoryIT {

	@Autowired
	@Qualifier("eventMongoTemplate")
	protected MongoOperations mongoOperations;

	/**
	 * Cleans up database
	 */
	protected void cleanUpDB(String collectionFormat) {
		for (String collection : mongoOperations.getCollectionNames()) {
			if (StringUtils.endsWithIgnoreCase(collection, collectionFormat)) {
				mongoOperations.dropCollection(collection);
			}
		}
	}

}
