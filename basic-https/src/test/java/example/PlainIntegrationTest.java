package example;

import org.apache.http.client.HttpClient;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.ssl.SSLContextBuilder;
// import com.baeldung.ssl.HttpsEnabledApplication;
// import org.junit.Test;
// import org.junit.runner.RunWith;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.web.client.RestTemplate;

import javax.net.ssl.SSLContext;
import java.util.Collections;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

@SpringBootTest(classes = example.Launcher.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8443" })
@PropertySource("classpath:application.properties")

public class PlainIntegrationTest {

	private static final String url = "https://localhost:8443/welcome";

	@Value("${trust.store}")
	private Resource trustStore;

	@Value("${trust.store.password}")
	private String trustStorePassword;

	// NOTE: Disabled attribute does not protect from encountering the error:
	// Web server failed to start. Port 8443 was already in use
	// @Disabled("Disabled...")
	@Test
	public void test1() throws Exception {
		ResponseEntity<String> response = customRestTemplate().getForEntity(url,
				String.class, Collections.emptyMap());
		// Certificate for <localhost> doesn't match any of the subject alternative
		// names: []
		// https://stackoverflow.com/questions/50928061/certificate-for-localhost-doesnt-match-any-of-the-subject-alternative-names
		assertThat(response.getBody(), is("welcome"));
		assertThat(response.getStatusCode(), is(HttpStatus.OK));
	}

	private RestTemplate customRestTemplate() throws Exception {
		SSLContext sslContext = new SSLContextBuilder().loadTrustMaterial(
				trustStore.getURL(), trustStorePassword.toCharArray()).build();
		SSLConnectionSocketFactory socketFactory = new SSLConnectionSocketFactory(
				sslContext);
		HttpClient httpClient = HttpClients.custom()
				.setSSLSocketFactory(socketFactory).build();
		HttpComponentsClientHttpRequestFactory factory = new HttpComponentsClientHttpRequestFactory(
				httpClient);
		return new RestTemplate(factory);
	}
}
