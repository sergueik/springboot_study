package example;

// import org.junit.Test;
// import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
// import org.springframework.test.context.junit4.SpringRunner;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;

//NOTE:
// Duplicate annotation of non-repeatable type @SpringBootTest. Only annotation types marked @Repeatable can be used multiple times at one target.
//org.springframework.boot.test.context.SpringBootTest is not a repeatable annotation type 
// @SpringBootTest

// @RunWith(SpringRunner.class)
@SpringBootTest
public class ApplicationTests {

	@Test
	public void contextLoads() {
	}

}
