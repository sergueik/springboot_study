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
// import org.elasticsearch.client.indices.CreateIndexRequest;
import org.elasticsearch.common.xcontent.XContentBuilder;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;
import java.net.InetAddress;

import static org.elasticsearch.common.xcontent.XContentFactory.jsonBuilder;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

public class ElasticsearchTest {

	private static final Logger logger = LogManager
			.getLogger(ElasticsearchTest.class);
	private static RestHighLevelClient client;
	private static final String INDEX = "scenario1";

	@BeforeClass
	public static void startElasticsearchRestClient() throws IOException {
		// does not currently have properties ser
		int testClusterPort = Integer
				.parseInt(System.getProperty("tests.cluster.port", "9200"));
		// use NAT
		String testClusterHost = System.getProperty("tests.cluster.host",
				InetAddress.getLocalHost().getHostAddress());
		String testClusterScheme = System.getProperty("tests.cluster.scheme",
				"http");
		String testClusterUser = System.getProperty("tests.cluster.user",
				"elastic");
		String testClusterPass = System.getProperty("tests.cluster.pass",
				"changeme");

		logger.info("Starting a client on {}://{}:{}", testClusterScheme,
				testClusterHost, testClusterPort);

		// We start a client
		RestClientBuilder builder = getClientBuilder(
				new HttpHost(testClusterHost, testClusterPort, testClusterScheme),
				testClusterUser, testClusterPass);
		client = new RestHighLevelClient(builder);

		// We make sure the cluster is running
		MainResponse info = client.info(RequestOptions.DEFAULT);
		logger.info("Client is running against an elasticsearch cluster {}.",
				info.getVersion().toString());
	}

	@AfterClass
	public static void stopElasticsearchRestClient() throws IOException {
		if (client != null) {
			logger.info("Closing elasticsearch client.");
			client.close();
		}
	}

	private static RestClientBuilder getClientBuilder(HttpHost host,
			String username, String password) {
		final CredentialsProvider credentialsProvider = new BasicCredentialsProvider();
		credentialsProvider.setCredentials(AuthScope.ANY,
				new UsernamePasswordCredentials(username, password));

		return RestClient.builder(host)
				.setHttpClientConfigCallback(httpClientBuilder -> httpClientBuilder
						.setDefaultCredentialsProvider(credentialsProvider));
	}

	@Test
	public void test1() throws IOException {
		// We remove any existing index
		try {
			logger.info("-> Removing index {}.", INDEX);
			client.indices().delete(new DeleteIndexRequest(INDEX),
					RequestOptions.DEFAULT);
		} catch (ElasticsearchStatusException e) {
			assertThat(e.status().getStatus(), is(404));
		}

		// We create a new index
		// need 7.x
		//
		// logger.info("-> Creating index {}.", INDEX);
		// client.indices().create(new CreateIndexRequest(INDEX),
		// RequestOptions.DEFAULT);

		// We index some documents
		logger.info("-> Indexing one document in {}.", INDEX);

		XContentBuilder data = jsonBuilder().startObject().field("foo", "bar")
				.endObject();
		logger.info("Sending " + data.contentType() + " "
				+ BytesReference.bytes(data).utf8ToString());

		// https://github.com/elastic/elasticsearch/blob/master/server/src/main/java/org/elasticsearch/action/index/IndexRequest.java#L375
		// https://github.com/elastic/elasticsearch/blob/v6.6.2/server/src/main/java/org/elasticsearch/action/index/IndexRequest.java#L389
		// java.lang.IllegalArgumentException: The number of object passed must be
		// even but was [1]
		IndexRequest indexRequest = new IndexRequest(INDEX).source(data,
				new Object[] { "foo", "bar"});

		try {
			IndexResponse ir = client.index(
					indexRequest.setRefreshPolicy(WriteRequest.RefreshPolicy.IMMEDIATE),
					RequestOptions.DEFAULT);
			logger.info("-> Document indexed with _id {}.", ir.getId());

		} catch (ActionRequestValidationException e) {
			// org.elasticsearch.action.ActionRequestValidationException: Validation
			// Failed: 1: type is missing;
			logger.info("Exception(ignored) {}", e.toString());
		}

		// We search
		try {
			SearchResponse sr = client.search(new SearchRequest(INDEX),
					RequestOptions.DEFAULT);
			logger.info("{}", sr);
			assertThat(sr.getHits().getTotalHits(), is(1L));
			// assertThat(sr.getHits().getTotalHits().value, is(1L));
		} catch (ElasticsearchStatusException e) {
			logger.info("Exception(ignored) {}", e.toString());
			// ElasticsearchStatusException[Elasticsearch exception
			// [type=illegal_argument_exception, reason=request [/scenario1/_search]
			// contains unrecognized parameter: [ccs_minimize_roundtrips]]]
			// https://stackoverflow.com/questions/55602377/elasticsearchstatusexception-contains-unrecognized-parameter-ccs-minimize-roun

			// ElaseticsearchStatusException[Elasticsearch exception
			// [type=index_not_found_exception,reason=no such index]]
		}
	}
}
