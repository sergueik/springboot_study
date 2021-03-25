package example.integration;

import org.apache.http.HttpHost;
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.UsernamePasswordCredentials;
import org.apache.http.client.CredentialsProvider;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.elasticsearch.client.ResponseException;
import org.elasticsearch.ElasticsearchStatusException;
import org.elasticsearch.action.ActionRequestValidationException;
import org.elasticsearch.action.admin.indices.delete.DeleteIndexRequest;
import org.elasticsearch.action.index.IndexRequest;
import org.elasticsearch.action.index.IndexResponse;
// NOTE:  change ot package needed after upgrade to 7.6.1
import org.elasticsearch.client.core.MainResponse;
import org.elasticsearch.action.search.SearchRequest;
import org.elasticsearch.action.search.SearchResponse;
import org.elasticsearch.action.support.WriteRequest;
import org.elasticsearch.client.Request;
import org.elasticsearch.client.RequestOptions;
import org.elasticsearch.client.RestClient;
import static org.elasticsearch.client.RestClient.builder;
import org.elasticsearch.client.RestClientBuilder;
import org.elasticsearch.client.RestHighLevelClient;
import org.elasticsearch.common.bytes.BytesReference;
import org.elasticsearch.client.indices.CreateIndexRequest;
import org.elasticsearch.common.xcontent.XContentBuilder;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.net.InetAddress;
import java.nio.file.attribute.AclEntry.Builder;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.io.FileInputStream;
import static org.elasticsearch.common.xcontent.XContentFactory.jsonBuilder;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

public class ElasticsearchTest {

	private static final Logger logger = LogManager
			.getLogger(ElasticsearchTest.class);
	private static RestHighLevelClient client = null;
	private static RestClient restClient = null;
	private static final String INDEX = "scenario1";
	private static final String INDEX2 = "scenario2";

	@BeforeClass
	public static void startElasticsearchRestClient() throws IOException {
		// does not currently have properties ser

		Properties properties = new Properties();
		InputStream input = null;
		String propertiesFile = "application.properties";
		// TODO: prepend server root
		input = new FileInputStream(String.format("%s/src/test/resources/%s",
				System.getProperty("user.dir"), propertiesFile));
		properties.load(input);
		int testClusterPort = Integer
				.parseInt(properties.getProperty("tests.cluster.port", "9200"));
		String testClusterHost = properties.getProperty("tests.cluster.host",
				InetAddress.getLocalHost().getHostAddress());
		String testClusterScheme = properties.getProperty("tests.cluster.scheme",
				"http");
		String testClusterUser = properties.getProperty("tests.cluster.user",
				"elastic");
		String testClusterPass = properties.getProperty("tests.cluster.pass",
				"changemenow");

		logger.info("Starting a client on {}://{}:{}", testClusterScheme,
				testClusterHost, testClusterPort);

		// start a client
		RestClientBuilder builder = getClientBuilder(
				new HttpHost(testClusterHost, testClusterPort, testClusterScheme),
				testClusterUser, testClusterPass);

		// Without xpack enabled the credentials are ignored
		logger.info("Starting a request builder with credentials {}:{}",
				testClusterUser, testClusterPass);

		client = new RestHighLevelClient(builder);

		// create a plain RestClient
		restClient = builder(
				new HttpHost(testClusterHost, testClusterPort, testClusterScheme))
						.build();
		// make sure the cluster is running
		try {
			RequestOptions options = RequestOptions.DEFAULT;
			MainResponse info = client.info(options);
			logger.info("Client is running against an elasticsearch cluster {}.",
					info.getVersion().toString());
		} catch (org.elasticsearch.client.ResponseException e) {
			logger.error("Response Exception: {}", e);
			// not reached possibly becauer authentication failure is handled via
			// RuntimeException
			// https://stackoverflow.com/questions/2190161/difference-between-java-lang-runtimeexception-and-java-lang-exception
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
	private static RestClientBuilder getClientBuilder(HttpHost host,
			String username, String password) {
		final CredentialsProvider credentialsProvider = new BasicCredentialsProvider();
		credentialsProvider.setCredentials(AuthScope.ANY,
				new UsernamePasswordCredentials(username, password));

		return RestClient.builder(host)
				.setHttpClientConfigCallback(httpClientBuilder -> httpClientBuilder
						.setDefaultCredentialsProvider(credentialsProvider));
		// can use both lambda or anonymous class syntax here
	}

	@Test
	public void test1() throws IOException {
		// remove existing index, to be sure in the number of entries found later
		try {
			logger.info("-> Removing index {}.", INDEX);
			client.indices().delete(new DeleteIndexRequest(INDEX),
					RequestOptions.DEFAULT);
		} catch (ElasticsearchStatusException e) {
			assertThat(e.status().getStatus(), is(404));
		}

		// create a new index
		// NOTE: needs 7.x
		logger.info("-> Creating index {}.", INDEX);
		client.indices().create(new CreateIndexRequest(INDEX),
				RequestOptions.DEFAULT);

		// index some documents
		logger.info("-> Indexing one document in {}.", INDEX);
		Map<String, Object> payloadData = new HashMap<>();
		payloadData.put("first", "string data");
		payloadData.put("second", 100);
		payloadData.put("third", true);
		Map<String, Object> param1 = new HashMap<>();
		param1.put("param1", "value1");
		param1.put("param2", "value2");
		param1.put("param3", "value3");
		payloadData.put("four", param1);
		XContentBuilder data = jsonBuilder().map(payloadData);
		// alternatively they have a set of chained methods
		// XContentBuilder data = jsonBuilder().startObject().field("key", "data")
		// .endObject();

		logger.info("Sending " + data.contentType() + " "
				+ BytesReference.bytes(data).utf8ToString());

		IndexRequest indexRequest = new IndexRequest(INDEX).source(data);
		try {
			IndexResponse ir = client.index(
					indexRequest.setRefreshPolicy(WriteRequest.RefreshPolicy.IMMEDIATE),
					RequestOptions.DEFAULT);
			logger.info("-> Document indexed with _id {}.", ir.getId());

		} catch (ActionRequestValidationException e) {
			logger.info("Exception(ignored) {}", e.toString());
		}

		// search
		try {
			SearchResponse searchResponse = client.search(new SearchRequest(INDEX),
					RequestOptions.DEFAULT);
			logger.info("Response: {}", searchResponse);
			assertThat(searchResponse.getHits().getTotalHits().value, is(1L));
			assertThat(searchResponse.getHits().getAt(0).getIndex(), is("scenario1"));
		} catch (ElasticsearchStatusException e) {
			logger.info("Exception(ignored) {}", e.toString());
		}
	}

	@Test
	public void test5() throws IOException {
		try {
			Request request = new Request("PUT", INDEX2 + "/");
			int statusCode = restClient.performRequest(request).getStatusLine()
					.getStatusCode();
			logger.info("-> Creating index status code {} .", statusCode);
			assertThat(statusCode, is(200));
		} catch (ResponseException e) {
			assertThat(e.getResponse().getStatusLine().getStatusCode(), is(400));
			logger.info("Got exception {}", e.getMessage());
		}
	}

	// POST KQL
	@Test
	public void test6() throws IOException {
		try {
			Request request = new Request("POST",
					INDEX2 + "/_update_by_query?conflicts=proceed&refresh");
			request.setJsonEntity("{\"script\":{}, \"query\":{}}");
			int statusCode = restClient.performRequest(request).getStatusLine()
					.getStatusCode();
			logger.info("-> Creating index status code {} .", statusCode);
			assertThat(statusCode, is(200));
		} catch (ResponseException e) {
			assertThat(e.getResponse().getStatusLine().getStatusCode(), is(400));
			logger.info("Got exception {}", e.getMessage());
		}
	}
}
