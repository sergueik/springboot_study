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

public class BrokenRestTemplateHelper {
	public static RestTemplate customRestTemplate() throws Exception {
		final int maxTotal = 10;
		final int defaultMaxPerRoute = 10;
		return customRestTemplate(maxTotal, defaultMaxPerRoute);
	}

	public static RestTemplate customRestTemplate(final int maxTotal,
			final int defaultMaxPerRoute) throws Exception {

		// establish "connection reuse" strategy
		// origin:
		// https://stackoverflow.com/questions/64810671/spring-boot-random-sslexception-connection-reset-in-kubernetes-with-jdk11
		final Registry<ConnectionSocketFactory> schemeRegistry = RegistryBuilder
				.<ConnectionSocketFactory> create()
				.register("http", PlainConnectionSocketFactory.getSocketFactory())
				.register("https", SSLConnectionSocketFactory.getSocketFactory())
				.build();

		// https://hc.apache.org/httpcomponents-client-4.5.x/current/httpclient/apidocs/org/apache/http/impl/conn/PoolingHttpClientConnectionManager.html#PoolingHttpClientConnectionManager(long,%20java.util.concurrent.TimeUnit)
		// NOTE: this constructor is failing to cope with the SSL trafic encryption
		final long timeToLive = 10;
		final TimeUnit timeUnit = TimeUnit.SECONDS;
		final PoolingHttpClientConnectionManager poolingHttpClientConnectionManager = new PoolingHttpClientConnectionManager(
				timeToLive, timeUnit);
		poolingHttpClientConnectionManager.setMaxTotal(maxTotal);
		poolingHttpClientConnectionManager
				.setDefaultMaxPerRoute(defaultMaxPerRoute);

		final CloseableHttpClient httpClient = HttpClients.custom()
				.setConnectionReuseStrategy(NoConnectionReuseStrategy.INSTANCE)
				.setConnectionManager(poolingHttpClientConnectionManager).build();
		// .setConnectTimeout(connectTimeout)
		// .setSocketTimeout(readTimeout)

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
