package example.service;

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

import javax.annotation.Resource;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.xml.sax.SAXException;
import org.yaml.snakeyaml.Yaml;

import example.dao.Dao;
import example.entity.Result;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

/*
 *  @author: Serguei Kouzmine (kouzmine_serguei@yahoo.com)
 */
public class BaseServiceTest {

	@Autowired
	private Dao dao;
	private BaseService service = new ServiceImpl();

	// https://stackoverflow.com/questions/266370/how-do-i-unit-test-jdbc-code-in-java
	@Disabled("incorrect DAO initialization attempt, receiving null")
	@Test
	public void test() throws Exception {
		Result result = service.findAllHost();
		assertThat(result, notNullValue());
		assertThat(result.getData(), is(2));
	}
}
