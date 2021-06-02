package example;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Map;

import example.YamlConfig;

import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

@SuppressWarnings("deprecation")
public class YamlConfigTest {

	private final String yamlFile = "application.yml";
	private final String propertiesFilePath = String.format("%s/target/classes", System.getProperty("user.dir"));
	private final String key = "browser";
	private final String value = System.getProperty(key, "chrome");
	private final InputStream resource = getClass().getClassLoader().getResourceAsStream(yamlFile);
	private YamlConfig config;

	@BeforeMethod
	public void setup() {
		YamlConfig.setDebug(true);
		config = YamlConfigLoader.getYamlConfig(yamlFile, propertiesFilePath);
	}

	@Test
	public void loadTest() {
		assertThat(config, notNullValue());
	}

	@Test(enabled = true)
	public void getMapTest() {
		Map<String, Object> map = config.getMap("configuration");
		assertThat(map, notNullValue());
		assertThat(map.keySet(), hasItems(new String[] { key }));
		assertThat(map.get(key), is(value));
	}

	@Test(enabled = true)
	public void getConfigurationTest1() {
		String value = config.getString(String.format("configuration.%s", key));
		assertThat(value, is(value));
	}

	@Test(enabled = true)
	public void getConfigurationTest2() {
		String browser = config.getString(String.format("configuration.%s", key));
		String value = config.getString(String.format("data.%s", browser));
		assertThat(value, is("value"));
	}

}