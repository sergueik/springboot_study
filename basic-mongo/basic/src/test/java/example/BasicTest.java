package example;

import java.net.UnknownHostException;
import java.util.List;
import java.util.Set;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.net.UnknownHostException;
import java.util.Collections;
import java.util.List;

import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.hamcrest.CoreMatchers.*;

import com.mongodb.DB;
import com.mongodb.MongoClient;
import org.bson.Document;
public class BasicTest {

	private static MongoClient mongoClient;

	@Before
	public void setUp() throws UnknownHostException {
		try {

			mongoClient = new MongoClient("localhost", 27717);

		} catch (UnknownHostException ex) {
			ex.printStackTrace();
		}

	}

	@Test
	public void shouldRunTest() {
		assertThat(true, is(true));
	}

	@Test
	public void listDatabases() {
		List<String> databases = mongoClient.getDatabaseNames();
		assertThat(databases, notNullValue());
		for (String dbName : databases) {
			System.out.println("- Database: " + dbName);

			DB db = mongoClient.getDB(dbName);
			assertThat(db, notNullValue());

			Set<String> collections = db.getCollectionNames();
			assertThat(collections, notNullValue());
			if (collections.size() == 0){
				System.out.println("No collections in: " + dbName);

			} else {
			for (String colName : collections) {
				System.out.println("\t + Collection: " + colName);
			}
}
		}

	}
	@Test
	public void createDatabase() {
String dbName = "myUserDb";
			DB db = mongoClient.getDB(dbName);
db.getCollection("agents").insertOne(new Document().append("name", "James"));

	}

	@After
	public void tearDown() {
		mongoClient.close();
	}
}
