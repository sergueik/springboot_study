package example.integration;

import org.apache.http.HttpHost;
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.UsernamePasswordCredentials;
import org.apache.http.client.CredentialsProvider;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.elasticsearch.ElasticsearchStatusException;
import org.elasticsearch.action.ActionRequestValidationException;
import org.elasticsearch.action.admin.indices.delete.DeleteIndexRequest;
import org.elasticsearch.action.index.IndexRequest;
import org.elasticsearch.action.index.IndexResponse;
import org.elasticsearch.action.main.MainResponse;
import org.elasticsearch.action.search.SearchRequest;
import org.elasticsearch.action.search.SearchResponse;
import org.elasticsearch.action.support.WriteRequest;
import org.elasticsearch.client.RequestOptions;
import org.elasticsearch.client.RestClient;
import org.elasticsearch.client.RestClientBuilder;
import org.elasticsearch.client.RestHighLevelClient;
import org.elasticsearch.common.bytes.BytesReference;
// not available in 6.2.0
import org.elasticsearch.client.indices.CreateIndexRequest;

import org.elasticsearch.common.xcontent.XContentBuilder;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.net.InetAddress;
import java.util.Properties;
import java.io.FileInputStream;
import static org.elasticsearch.common.xcontent.XContentFactory.jsonBuilder;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

public class ElasticsearchTest {

	private static final Logger logger = LogManager.getLogger(ElasticsearchTest.class);
	private static RestHighLevelClient client;
	private static final String INDEX = "scenario1";

	@BeforeClass
	public static void startElasticsearchRestClient() throws IOException {
		// does not currently have properties ser

		Properties properties = new Properties();
		InputStream input = null;
		String propertiesFile = "application.properties";
		// TODO: prepend server root
		input = new FileInputStream(
				String.format("%s/src/test/resources/%s", System.getProperty("user.dir"), propertiesFile));
		properties.load(input);
		int testClusterPort = Integer.parseInt(properties.getProperty("tests.cluster.port", "9200"));
		String testClusterHost = properties.getProperty("tests.cluster.host",
				InetAddress.getLocalHost().getHostAddress());
		String testClusterScheme = properties.getProperty("tests.cluster.scheme", "http");
		String testClusterUser = properties.getProperty("tests.cluster.user", "elastic");
		String testClusterPass = properties.getProperty("tests.cluster.pass", "changemenow");

		logger.info("Starting a client on {}://{}:{}", testClusterScheme, testClusterHost, testClusterPort);

		// start a client
		RestClientBuilder builder = getClientBuilder(new HttpHost(testClusterHost, testClusterPort, testClusterScheme),
				testClusterUser, testClusterPass);

		// Without xpack enabled the credentials are ignored
		logger.info("Starting a request builder with credentials {}:{}", testClusterUser, testClusterPass);

		client = new RestHighLevelClient(builder);

		// make sure the cluster is running
		try {
			MainResponse info = client.info(RequestOptions.DEFAULT);
			logger.info("Client is running against an elasticsearch cluster {}.", info.getVersion().toString());
		} catch (org.elasticsearch.client.ResponseException e) {
			logger.error("Response Exception: {}", e);
			// not reached
			throw new RuntimeException(e);

		}
	}

	@AfterClass
	public static void stopElasticsearchRestClient() throws IOException {
		if (client != null) {
			logger.info("Closing elasticsearch client.");
			client.close();
		}
	}

	// see
	// https://github.com/elastic/elasticsearch/blob/master/client/rest/src/main/java/org/elasticsearch/client/RestClientBuilder.java
	private static RestClientBuilder getClientBuilder(HttpHost host, String username, String password) {
		final CredentialsProvider credentialsProvider = new BasicCredentialsProvider();
		credentialsProvider.setCredentials(AuthScope.ANY, new UsernamePasswordCredentials(username, password));

		return RestClient.builder(host).setHttpClientConfigCallback(
				httpClientBuilder -> httpClientBuilder.setDefaultCredentialsProvider(credentialsProvider));
	}

	@Test
	public void test1() throws IOException {
		// remove existing index, to be sure in the number of entries found later
		try {
			logger.info("-> Removing index {}.", INDEX);
			client.indices().delete(new DeleteIndexRequest(INDEX), RequestOptions.DEFAULT);
		} catch (ElasticsearchStatusException e) {
			assertThat(e.status().getStatus(), is(404));
		}

		// create a new index
		// NOTE: needs 7.x
		logger.info("-> Creating index {}.", INDEX);
		client.indices().create(new CreateIndexRequest(INDEX), RequestOptions.DEFAULT);

		// index some documents
		logger.info("-> Indexing one document in {}.", INDEX);
		// NOTE: the data information appears to be ignored, need use some other API
		XContentBuilder data = jsonBuilder().startObject().field("key", "data").endObject();
		logger.info("Sending " + data.contentType() + " " + BytesReference.bytes(data).utf8ToString());

		// https://github.com/elastic/elasticsearch/blob/master/server/src/main/java/org/elasticsearch/action/index/IndexRequest.java#L375
		// https://github.com/elastic/elasticsearch/blob/v6.6.2/server/src/main/java/org/elasticsearch/action/index/IndexRequest.java#L389
		// java.lang.IllegalArgumentException: The number of object passed must be
		// even but was [1]
		// filled the missing argument to have call go through
		IndexRequest indexRequest = new IndexRequest(INDEX).source(data,
				new Object[] { "information about the test", "more information", null, null });

		try {
			IndexResponse ir = client.index(indexRequest.setRefreshPolicy(WriteRequest.RefreshPolicy.IMMEDIATE),
					RequestOptions.DEFAULT);
			logger.info("-> Document indexed with _id {}.", ir.getId());

		} catch (ActionRequestValidationException e) {
			logger.info("Exception(ignored) {}", e.toString());
		}

		// search
		try {
			SearchResponse searchResponse = client.search(new SearchRequest(INDEX), RequestOptions.DEFAULT);
			logger.info("Response: {}", searchResponse);
			assertThat(searchResponse.getHits().getTotalHits().value, is(1L));
			assertThat(searchResponse.getHits().getAt(0).getIndex(), is("scenario1"));
		} catch (ElasticsearchStatusException e) {
			logger.info("Exception(ignored) {}", e.toString());
		}
	}
}
