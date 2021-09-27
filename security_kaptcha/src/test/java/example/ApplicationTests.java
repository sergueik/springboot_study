
package example;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.embedded.LocalServerPort;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.*;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.Arrays;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.endsWith;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;

import static org.hamcrest.MatcherAssert.assertThat;

// import static org.assertj.core.api.Assertions.assertThat;

@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
@DirtiesContext
public class ApplicationTests {

	@Autowired
	private TestRestTemplate restTemplate;

	@LocalServerPort
	private int port;

	@Test
	public void testHome() throws Exception {
		HttpHeaders headers = new HttpHeaders();
		headers.setAccept(Arrays.asList(MediaType.TEXT_HTML));
		ResponseEntity<String> entity = restTemplate.exchange("/", HttpMethod.GET,
				new HttpEntity<Void>(headers), String.class);
		assertThat(entity.getStatusCode(), is(HttpStatus.FOUND));
		assertThat(entity.getHeaders().getLocation().toString(),
				endsWith(this.port + "/login"));
	}

	@Test
	public void testLoginPage() throws Exception {
		HttpHeaders headers = new HttpHeaders();
		headers.setAccept(Arrays.asList(MediaType.TEXT_HTML));
		ResponseEntity<String> entity = restTemplate.exchange("/login",
				HttpMethod.GET, new HttpEntity<Void>(headers), String.class);
		assertThat(entity.getStatusCode(), is(HttpStatus.OK));
		assertThat(entity.getBody(), containsString("_csrf"));
	}

	@Test
	public void testHeaders() {
		getHeaders();
	}

	private HttpHeaders getHeaders() {
		HttpHeaders headers = new HttpHeaders();
		ResponseEntity<String> page = restTemplate.getForEntity("/login",
				String.class);
		assertThat(page.getStatusCode(), is(HttpStatus.OK));
		String cookie = page.getHeaders().getFirst("Set-Cookie");
		headers.set("Cookie", cookie);
		Pattern pattern = Pattern
				.compile("(?s).*name=\"_csrf\".*?value=\"([^\"]+).*");
		Matcher matcher = pattern.matcher(page.getBody());
		assertThat(matcher.matches(), is(true));
		headers.set("X-CSRF-TOKEN", matcher.group(1));
		return headers;
	}

	@Test
	public void testCss() throws Exception {
		ResponseEntity<String> entity = restTemplate
				.getForEntity("/css/bootstrap.min.css", String.class);
		assertThat(entity.getStatusCode(), is(HttpStatus.OK));
		assertThat(entity.getBody(), containsString("body"));
	}

	@Test
	public void test3() throws Exception {
		ResponseEntity<String> entity = restTemplate.getForEntity("/js/jquery.js",
				String.class);
		assertThat(entity.getStatusCode(), is(HttpStatus.OK));
		assertThat(entity.getBody(), containsString("jQuery"));
	}

	@Test
	public void test4() throws Exception {
		ResponseEntity<String> entity = restTemplate.getForEntity("/someplace",
				String.class);
		assertThat(entity.getStatusCode(), is(HttpStatus.FOUND));
		assertThat(entity.hasBody(), is(false));
		assertThat(entity.getHeaders().get("Location").toString(),
				containsString("/login"));
	}

	@Test
	public void test5() throws Exception {
		ResponseEntity<String> entity = restTemplate.postForEntity("/login", null,
				null);
		assertThat(entity.getStatusCode(), is(HttpStatus.FORBIDDEN));
	}

}
