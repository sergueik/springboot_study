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

import com.mongodb.client.MongoDatabase;
import com.mongodb.client.MongoIterable;
import com.mongodb.MongoClient;
import org.bson.Document;

public class BasicTest {

	private static MongoClient mongoClient;

	@Before
	public void setUp() throws UnknownHostException {

		mongoClient = new MongoClient("localhost", 27717);

	}

	@Test
	public void shouldRunTest() {
		assertThat(true, is(true));
	}

	@Test
	public void listDatabases() {

		MongoIterable<String> databases = mongoClient.listDatabaseNames();
		assertThat(databases, notNullValue());
		for (String dbName : databases) {
			System.out.println("- Database: " + dbName);

			MongoDatabase db = mongoClient.getDatabase(dbName);
			assertThat(db, notNullValue());

			MongoIterable<String> collections = db.listCollectionNames();
			assertThat(collections, notNullValue());
			if (!collections.iterator().hasNext()) {
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
		MongoDatabase db = mongoClient.getDatabase(dbName);
		db.getCollection("agents").insertOne(new Document().append("name", "James"));

	}

	@After
	public void tearDown() {
		mongoClient.close();
	}
}
