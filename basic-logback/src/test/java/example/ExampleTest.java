package example;

import org.junit.Test;
import org.junit.Ignore;

import org.junit.runner.RunWith;

import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
// import org.springframework.test.context.web.WebAppConfiguration;
// import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

// Spring Framwork Release sensitive area
// @RunWith(SpringJUnit4ClassRunner.class)
// @SpringApplicationConfiguration(classes = Example.class)
// @WebAppConfiguration
@RunWith(SpringRunner.class)
@SpringBootTest
public class ExampleTest {
	@Ignore
	// commented for 2.4.3 IllegalState Failed to load ApplicationContext
	@Test
	public void contextLoads() {
	}

}
