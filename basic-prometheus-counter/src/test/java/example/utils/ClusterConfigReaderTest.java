package example.utils;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;

import java.nio.file.Files;
import java.nio.file.Paths;

import java.lang.reflect.Field;
import java.lang.reflect.Type;

import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.jupiter.api.Test;
import org.xml.sax.SAXException;
import org.yaml.snakeyaml.Yaml;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

public class ClusterConfigReaderTest {

	private ClusterConfigReader snakeYamlReader = new ClusterConfigReader();
	Map<String, Host> info = new HashMap<>();
	String fileName = "cluster.yaml";
	String encoding = "UTF-8";

	@Test
	public void test() throws Exception {
		snakeYamlReader.read(fileName);
		info = snakeYamlReader.getInfo();
		assertThat(info, notNullValue());
		assertThat(info.keySet().size(), is(5));
	}
}
