package example;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonParser;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.PathNotFoundException;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.core.IsNot.not;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import example.CopyBookYamlLoader;
import example.FindMatch;

public class CopyBookSpecYamlLoaderTest {
	private static final String copyBook = "BR002202312251111222233WDL000023456EUR";

	@SuppressWarnings("unchecked")
	@DisplayName("Verify the results filtering")
	@Test
	public void test1() {

		CopyBookSpec copyBookSpec = CopyBookYamlLoader.loadFromResource("copybook.yaml");
		Pattern pattern = RegexFromCopyBook.compile(copyBookSpec);
		System.err.println(RegexFromCopyBook.buildRegex(copyBookSpec));
		System.err.println(copyBook);
		Matcher matcher = pattern.matcher(copyBook);

		if (!matcher.matches()) {
			throw new IllegalStateException("Record does not match copybook");
		}

		String account = matcher.group("ACCOUNT");

	}
}
