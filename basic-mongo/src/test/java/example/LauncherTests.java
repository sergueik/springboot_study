package example;

import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;

@RunWith(SpringRunner.class)
@SpringBootTest
public class LauncherTests {
	@MockBean
	ModelMongoRepository mongoRepository;

	// NOTE: the mongo backend is not mocked in this test
	// if docker container is not operational 
	// exception in monitor thread while connecting to server mongo:27017 will be observed

	// @Ignore
	@Test
	public void contextLoads() {
	}

}
