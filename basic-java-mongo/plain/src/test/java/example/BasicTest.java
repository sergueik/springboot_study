package example;

import static com.mongodb.client.model.Filters.eq;
import static com.mongodb.client.model.Filters.or;
import static com.mongodb.client.model.Projections.fields;
import static com.mongodb.client.model.Projections.include;
import static org.hamcrest.CoreMatchers.anyOf;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.Level;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import org.bson.BsonValue;
import org.bson.Document;
import org.bson.conversions.Bson;
import org.bson.types.ObjectId;

import com.mongodb.BasicDBObject;
import com.mongodb.Block;
import com.mongodb.ConnectionString;
import com.mongodb.client.FindIterable;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoCursor;
import com.mongodb.client.MongoDatabase;
import com.mongodb.client.MongoIterable;
import com.mongodb.client.model.Filters;
import com.mongodb.client.model.Projections;
import com.mongodb.client.model.Sorts;
import com.mongodb.client.model.UpdateOptions;
import com.mongodb.client.model.Updates;
import com.mongodb.client.result.UpdateResult;
import java.util.Properties;

@SuppressWarnings("deprecation")
public class BasicTest {

	private static MongoClient mongoClient;
	private static MongoDatabase db;
	private static MongoCollection<Document> collection;
	private static FindIterable<Document> find;
	private static MongoCursor<Document> cursor;
	private static Document document;
	private static UpdateResult result;
	private static Document search;
	private static BasicDBObject basicDBObject;
	private static ObjectId documentId;
	private static String dbName = "myUserDb";
	private static Properties p = new Properties();
	private static String mongoServer = null;
	private static final Map<String, String> nameChange = new HashMap<>();
	static {
		nameChange.put("James", "Fred");
		nameChange.put("Fred", "James");
	};

	private static final Logger logger = LogManager.getLogger(BasicTest.class);

	@Before
	public void setUp() {
		try {

			// Map<String, String> p2 =
			// getProperties("src/test/resources/application.properties");
			// logger.info("properties: {}", p2.keySet());
			p.load(new FileInputStream("src/test/resources/application.properties"));
			logger.info("Loaded properties");
		} catch (IOException e) {
			System.err.println("Failed to load properties");
		}
		mongoServer = p.getProperty("mongo.server", "localhost");
		ConnectionString connectionString = new ConnectionString(
				// NOTE: not using default port
				String.format("mongodb://%s:%d", mongoServer, 27017));
		logger.info("Connectiong to: " + connectionString.getConnectionString());
		mongoClient = MongoClients.create(connectionString);
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
					logger.log(Level.ALL, "\t + Collection: {}", colName);
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

	// @Ignore
	@Test
	public void insertMultipleDocuments() {
		db = mongoClient.getDatabase(dbName);
		logger.info("insertMultipleDocuments started.");
		final int maxcnt = 10000;
		logger.info("insert {} documents.", maxcnt);
		for (int cnt = 0; cnt < maxcnt; cnt++) {
			String item = String.format("bar%d", cnt);
			document = new Document();
			Map<String, Object> size = new HashMap<>();
			size.put("h", 28);
			size.put("w", 35.5);
			size.put("oum", "cm");
			document.append("item", item).append("qty", 100)
					.append("tags", Arrays.asList(new String[] { "cotton" }))
					.append("size", size);
			db.getCollection("foo").insertOne(document);
		}
		logger.info("insertMultipleDocuments completed.");
	}

	@Test
	public void insertManyDocuments() {
		db = mongoClient.getDatabase(dbName);
		List<Document> list = new ArrayList<>();
		for (int i = 0; i < 5; i++) {
			document = new Document();
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
		cursor = find.sort(Sorts.ascending("name")).limit(2).iterator();
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
	@Ignore("failng with java.lang.AssertionError")
	@Test
	public void projections() {
		db = mongoClient.getDatabase(dbName);
		collection = db.getCollection("agents");
		find = collection.find(new Document());
		assertThat(find, notNullValue());

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
	@Ignore
	@Test
	public void filterDocumentsTest() {
		db = mongoClient.getDatabase(dbName);
		collection = db.getCollection("agents");
		Bson filter = Filters.eq("name", "James");
		find = collection.find(filter);
		assertThat(find, notNullValue());
		document = find.first();
		// TODO: currently is failing
		assertThat(document, notNullValue());
		logger.info(document.toJson());
	}

	@Test
	public void filterStaticBuilderTest() {
		db = mongoClient.getDatabase(dbName);
		collection = db.getCollection("agents");
		find = collection.find(or(eq("name", "James"), eq("name", "Fred")));

		Block<Document> codeBlock = new Block<Document>() {
			@Override
			public void apply(final Document document) {
				logger.info(document.toJson());
			}
		};
		find.forEach(codeBlock);
	}

	// TODO: triage
	@Ignore
	@Test
	public void findDocumentArgumentTest() {
		db = mongoClient.getDatabase(dbName);
		collection = db.getCollection("agents");
		find = collection
				.find(new Document("age", new Document("$gte", 20).append("$lt", 30)));
		assertThat(find, notNullValue());
		document = find.first();
		assertThat(document, notNullValue());
		logger.info(document.toJson());
	}

	// NOTE: commenting other tests will lead this one to fail with NPE
	// @Ignore
	// https://docs.mongodb.com/manual/reference/operator/query-comparison/
	// https://docs.mongodb.com/manual/reference/operator/query/where/
	@Test
	public void updateDocumentTest() {
		db = mongoClient.getDatabase(dbName);
		collection = db.getCollection("agents");
		List<Document> d1 = new ArrayList<>();
		// com.mongodb.MongoWriteException: unknown top level operator: $eq
		d1.add(new Document("name", new Document("$eq", "James")));
		d1.add(new Document("name", new Document("$eq", "Fred")));
		search = new Document();
		search.put("$or", d1);
		document = collection.find(search).first();
		logger.info(document.toJson());
		documentId = document.get("_id", ObjectId.class);
		logger.info("Save id: " + documentId.toString());

		String newName = nameChange
				.get(collection.find(search).first().get("name", String.class));
		result = collection.updateOne(search, Updates.set("name", newName),
				new UpdateOptions().upsert(true).bypassDocumentValidation(true));
		assertThat(result, notNullValue());
		long count = result.getModifiedCount();
		assertThat(count, anyOf(equalTo(1L), equalTo(0L)));
		if (count > 0) {
			// NOTE: getUpsertedId is not the id of the document
			BsonValue id = result.getUpsertedId();
			logger.info("upsertedid: " + id);
			// convert to String to illustrate the recommended calling DSL
			document = collection.find(eq("_id", new ObjectId(documentId.toString())))
					.first();
			assertThat(document, notNullValue());
			assertThat(document.get("name", String.class), is(newName));
			logger.info(document.toJson());
		}
	}

	@Test
	public void findByKey() {
		db = mongoClient.getDatabase(dbName);
		collection = db.getCollection("agents");

		basicDBObject = new BasicDBObject();
		basicDBObject.put("name", "James");
		find = collection.find(basicDBObject, Document.class);
	}

	@SuppressWarnings("serial")
	@Test
	public void findByDocumentArgument() {
		db = mongoClient.getDatabase(dbName);
		collection = db.getCollection("agents");

		search = new Document("$or", new ArrayList<Document>() {
			{
				add(new Document("name", new Document("$eq", "James")));
				add(new Document("name", new Document("$eq", "Fred")));
			}
		});
		find = collection.find(search, Document.class);
		assertThat(find, notNullValue());
		document = find.first();
		assertThat(document, notNullValue());
		logger.info(document.toJson());
	}

	@Test
	public void findByExample() {
		db = mongoClient.getDatabase(dbName);
		collection = db.getCollection("agents");
		collection.insertOne(new Document("name",
				new Document("first", "James").append("last", "Bond")));
		document = collection
				.find(new Document("name.first", "James"), Document.class).first();
		assertThat(document, notNullValue());
		logger.info(document.toJson());

		document = collection.find(new Document("name.first",
				new Document("$regex", "^j.*").append("$options", "i"))).first();
		assertThat(document, notNullValue());
		logger.info(document.toJson());
		document = collection
				.find(new Document("name.first",
						new Document("$regex", "^j.*").append("$options", "i")))
				.projection(new Document("name", 1)).first();
		assertThat(document, notNullValue());
		logger.info(document.toJson());
	}

	// Query and Critera are defined in SpringData, not core MongoDB
	@After
	public void tearDown() {
		mongoClient.close();
	}

	public static Map<String, String> getProperties(final String fileName) {
		Properties p = new Properties();
		Map<String, String> propertiesMap = new HashMap<>();
		// System.err.println(String.format("Reading properties file: '%s'",
		// fileName));
		try {
			p.load(new FileInputStream(fileName));
			@SuppressWarnings("unchecked")
			Enumeration<String> e = (Enumeration<String>) p.propertyNames();
			for (; e.hasMoreElements();) {
				String key = e.nextElement();
				String val = p.get(key).toString();
				System.out.println(String.format("Reading: '%s' = '%s'", key, val));
				propertiesMap.put(key, val);
			}

		} catch (FileNotFoundException e) {
			System.err.println(
					String.format("Properties file was not found: '%s'", fileName));
			e.printStackTrace();
		} catch (IOException e) {
			System.err.println(
					String.format("Properties file is not readable: '%s'", fileName));
			e.printStackTrace();
		}
		return (propertiesMap);
	}

}
