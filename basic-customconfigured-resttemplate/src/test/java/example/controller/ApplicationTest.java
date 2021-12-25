package example.controller;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.web.client.RestTemplate;

import example.controller.config.HttpClientConfig;
import example.controller.config.RestTemplateConfig;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = { RestTemplateConfig.class,
		HttpClientConfig.class })
public class ApplicationTest {

	@Autowired
	RestTemplate restTemplate;
	final static int port = 8085;
	final static String route = "/basic";
	final static String body = "Hello basic";
	final String uri = String.format("http://localhost:%d/%s", port, route);

	@Test
	public void test() {
		String result = restTemplate.getForObject(uri, String.class);
		Assert.assertEquals(true, result.indexOf(body) > 0);
	}
}
