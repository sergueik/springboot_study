package example;

// bb926d3bdeefc8de38987e8ac97ceefbaf23d6ac
import example.Application;
/*
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
*/
import org.junit.Test;
import org.junit.Ignore;

import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.vault.repository.configuration.EnableVaultRepositories;

/**
 * NOTE: This  test will fail if the vault server is not up and running 
 *
 */
@RunWith(SpringRunner.class)
@SpringBootTest(classes = Application.class)
@EnableVaultRepositories
public class BasicTest {

    @Test
    @Ignore
    public void test() {
    }
}
