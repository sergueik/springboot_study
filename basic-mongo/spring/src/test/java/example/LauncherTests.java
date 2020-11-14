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

	// TODO: mock the mongo backend
	// Exception in monitor thread while connecting to server mongo:27017

	@Ignore
	@Test
	public void contextLoads() {
	}

}
