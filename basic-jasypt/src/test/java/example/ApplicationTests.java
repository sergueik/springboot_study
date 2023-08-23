package example;

import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

@RunWith(SpringRunner.class)
@SpringBootTest
public class ApplicationTests {

	@Ignore
	// TODO: condition. O Windows
	// receiving the exception:
	// Caused by: org.jasypt.exceptions.EncryptionOperationNotPossibleException:
	// Encryption raised an exception. A possible cause is you are using strong
	// encryption algorithms and you have not installed the Java Cryptography
	// Extension (JCE) Unlimited Strength Jurisdiction Policy Files in this Java
	// Virtual Machine
	@Test
	public void contextLoads() {
	}

}
