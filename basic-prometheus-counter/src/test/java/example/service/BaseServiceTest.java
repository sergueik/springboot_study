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

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.xml.sax.SAXException;
import org.yaml.snakeyaml.Yaml;

import example.entity.Result;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

public class BaseServiceTest {

	private BaseService service = new ServiceImpl();

	@Test
	public void test() throws Exception {
		String name = "John";

		// Result result = service.findStudentByName(name);
		Result result = service.findAllStudent();
		assertThat(result, notNullValue());
		assertThat(result.getData(), is(5));
	}
}
