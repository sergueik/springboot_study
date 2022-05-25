package example;

import java.util.concurrent.TimeUnit;

import javax.net.ssl.SSLContext;

import org.apache.http.client.HttpClient;
import org.apache.http.config.Registry;
import org.apache.http.config.RegistryBuilder;
import org.apache.http.conn.socket.ConnectionSocketFactory;
import org.apache.http.conn.socket.PlainConnectionSocketFactory;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.impl.NoConnectionReuseStrategy;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.impl.conn.PoolingHttpClientConnectionManager;
import org.apache.http.ssl.SSLContextBuilder;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.core.io.Resource;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;

public class CustomRestTemplateHelper {
	public static RestTemplate customRestTemplate(final Resource trustStore,
			final String trustStorePassword) throws Exception {
		final int maxTotal = 10;
		final int defaultMaxPerRoute = 10;
		return customRestTemplate(trustStore, trustStorePassword, maxTotal,
				defaultMaxPerRoute);
	}

	public static RestTemplate customRestTemplate(final Resource trustStore,
			final String trustStorePassword, final int maxTotal,
			final int defaultMaxPerRoute) throws Exception {

		SSLContext sslContext = new SSLContextBuilder().loadTrustMaterial(
				trustStore.getURL(), trustStorePassword.toCharArray()).build();
		SSLConnectionSocketFactory socketFactory = new SSLConnectionSocketFactory(
				sslContext);

		// establish "connection reuse" strategy
		// origin:
		// https://stackoverflow.com/questions/64810671/spring-boot-random-sslexception-connection-reset-in-kubernetes-with-jdk11
		final Registry<ConnectionSocketFactory> schemeRegistry = RegistryBuilder
				.<ConnectionSocketFactory> create()
				.register("http", PlainConnectionSocketFactory.getSocketFactory())
				.register("https", socketFactory).build();
		final PoolingHttpClientConnectionManager poolingHttpClientConnectionManager = new PoolingHttpClientConnectionManager(
				schemeRegistry);

		poolingHttpClientConnectionManager.setMaxTotal(maxTotal);
		poolingHttpClientConnectionManager
				.setDefaultMaxPerRoute(defaultMaxPerRoute);

		CloseableHttpClient httpClient = HttpClients.custom()
				.setSSLSocketFactory(socketFactory)
				.setConnectionReuseStrategy(NoConnectionReuseStrategy.INSTANCE)
				.setConnectionManager(poolingHttpClientConnectionManager).build();

		HttpComponentsClientHttpRequestFactory factory = new HttpComponentsClientHttpRequestFactory(
				httpClient);
		return new RestTemplate(factory);
		// equivalent:
		/*
		return new RestTemplateBuilder()
				.requestFactory(
						() -> new HttpComponentsClientHttpRequestFactory(httpClient))
				.build();
				*/
	}
}
