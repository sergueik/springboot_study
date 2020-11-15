package example;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;
import static org.hamcrest.CoreMatchers.*;

import com.mongodb.BasicDBObject;
import com.mongodb.Block;
import com.mongodb.client.FindIterable;
import com.mongodb.client.MongoClients;
import com.mongodb.client.MongoClient;
import com.mongodb.ConnectionString;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoCursor;
import com.mongodb.client.MongoDatabase;
import com.mongodb.client.MongoIterable;
import com.mongodb.client.model.Filters;
import com.mongodb.client.model.Projections;

import static com.mongodb.client.model.Filters.eq;
import static com.mongodb.client.model.Filters.and;

import static com.mongodb.client.model.Projections.include;
import static com.mongodb.client.model.Projections.fields;

import org.bson.Document;
import org.bson.conversions.Bson;
import org.apache.log4j.Logger;

public class BasicTest {

	private static MongoClient mongoClient;
	private static MongoDatabase db;
	private static MongoCollection<Document> collection;
	private static FindIterable<Document> find;
	private static MongoCursor<Document> cursor;
	private static String dbName = "myUserDb";

	private static final Logger logger = Logger.getLogger(BasicTest.class);

	@Before
	public void setUp() {
		ConnectionString connectionString = new ConnectionString(
				// NOTE: not using default port
				String.format("mongodb://%s:%d", "localhost", 27717));
		mongoClient = MongoClients.create(connectionString);
	}

	@Test
	public void dummyTest() {
		assertThat(true, is(true));
	}

	@Test
	public void listDatabases() {

		MongoIterable<String> databases = mongoClient.listDatabaseNames();
		assertThat(databases, notNullValue());
		for (String dbName : databases) {
			logger.info("- Database: " + dbName);

			MongoDatabase db = mongoClient.getDatabase(dbName);
			assertThat(db, notNullValue());

			MongoIterable<String> collections = db.listCollectionNames();
			assertThat(collections, notNullValue());
			if (!collections.iterator().hasNext()) {
				logger.info("No collections in: " + dbName);

			} else {
				for (String colName : collections) {
					logger.info("\t + Collection: " + colName);
				}
			}
		}

	}

	@Test
	public void createDocument() {
		db = mongoClient.getDatabase(dbName);
		db.getCollection("agents")
				.insertOne(new Document().append("name", "James"));
	}

	@Test
	public void insertManyDocuments() {
		db = mongoClient.getDatabase(dbName);
		List<Document> list = new ArrayList<>();
		for (int i = 0; i < 5; i++) {
			Document document = new Document();
			document.append("name", "name " + i).append("age", 20 + i).append(
					"topics", Arrays.asList(new String[] { "music", "travel", "eat" }));
			list.add(document);
		}
		db.getCollection("agents").insertMany(list);
	}

	@Test
	public void allDocuments() {
		db = mongoClient.getDatabase(dbName);
		collection = db.getCollection("agents");
		find = collection.find(new Document());
		assertThat(find, notNullValue());
		cursor = find.iterator();
		assertThat(cursor.hasNext(), is(true));
		try {
			while (cursor.hasNext()) {
				logger.info(cursor.next().toJson());
			}
		} finally {
			cursor.close();
		}
	}

	// http://www.java2s.com/example/java-api/com/mongodb/client/model/projections/fields-1-0.html
	@Test
	public void projections() {
		db = mongoClient.getDatabase(dbName);
		collection = db.getCollection("agents");
		find = collection.find(new Document());
		assertThat(find, notNullValue());
		boolean excludeId = false;
		cursor = find.projection(fields(include("name"), Projections.excludeId()))
				.iterator();
		assertThat(cursor.hasNext(), is(true));
		try {
			while (cursor.hasNext()) {
				logger.info(cursor.next().toJson());
			}
		} finally {
			cursor.close();
		}
	}

	// based on:
	// https://examples.javacodegeeks.com/software-development/mongodb/java-mongodb-query-document-example/
	// https://mongodb.github.io/mongo-java-driver/3.9/driver/getting-started/quick-start/
	@Test
	public void filterDocumentsTest() {
		db = mongoClient.getDatabase(dbName);
		collection = db.getCollection("agents");
		Bson filter = Filters.eq("name", "James");
		find = collection.find(filter);
		assertThat(find, notNullValue());
		Document document = find.first();
		assertThat(document, notNullValue());
		logger.info(document.toJson());
	}

	@SuppressWarnings("deprecation")
	@Test
	public void filterStaticBuilderTest() {
		db = mongoClient.getDatabase(dbName);
		collection = db.getCollection("agents");
		find = collection.find(and(eq("name", "James"), eq("name", "James")));

		Block<Document> codeBlock = new Block<Document>() {
			@Override
			public void apply(final Document document) {
				logger.info(document.toJson());
			}
		};
		find.forEach(codeBlock);
	}

	@Test
	public void findDocumentArgumentTest() {
		db = mongoClient.getDatabase(dbName);
		collection = db.getCollection("agents");
		find = collection
				.find(new Document("age", new Document("$gte", 20).append("$lt", 30)));
		assertThat(find, notNullValue());
		Document document = find.first();
		assertThat(document, notNullValue());
		logger.info(document.toJson());
	}

	@Test
	public void findByKey() {
		db = mongoClient.getDatabase(dbName);
		collection = db.getCollection("agents");
		BasicDBObject basicDBObject = new BasicDBObject();
		basicDBObject.put("name", "James");
		find = collection.find(basicDBObject, Document.class);
	}

	// Query and Critera are defined in SpringData, not core MongoDB
	@After
	public void tearDown() {
		mongoClient.close();
	}
}

