package example;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.context.TestPropertySource;
// import org.springframework.test.context.web.WebAppConfiguration;
// import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringRunner.class)
@SpringBootTest
@TestPropertySource(value = { "classpath:application.properties" })
public class AppTest {

	@Test
	public void contextLoads() {
	}

}
