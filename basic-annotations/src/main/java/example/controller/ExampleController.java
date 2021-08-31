package example.controller;

import java.util.List;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/")
public class ExampleController {

	private static final Logger logger = LogManager.getLogger(ExampleController.class);

	@Value("static value")
	private static String staticValue;

	@Value("fixed value")
	private String fixedValue;

	@Value("${message}")
	private String message;

	@Value("${missing: default value}")
	private String defValueProperty;

	@Value("${data.list}")
	private List<String> dataList;

	@Value("#{${object.details}}")
	private Map<String, String> complexObjectProperty;

	@GetMapping("employee")
	public String employeeInfo() {
		try {
			logger.info("staticValue=" + staticValue + " message=" + message + " property with defaul value="
					+ defValueProperty);
			return toString();
		} catch (NullPointerException e) {
			// TODO: provide properties definition under test
			logger.info("Exception (ignored): " + e.toString());
			return null;
		}
	}

	@Override
	public String toString() {
		return "Static variable=" + staticValue + "<br/>\n" + "Fixed value=" + fixedValue + "<br/>\n" + "Value Message="
				+ message + "<br/>\n" + "Property with Default value=" + defValueProperty + "<br/>\n" + "List values="
				+ dataList + "<br/>\n" + "Size of list=" + dataList.size() + "<br/>\n" + "Object value="
				+ complexObjectProperty + "\n";
	}

}
