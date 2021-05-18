package example.component;

import java.util.Properties;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class Example3Component {

//	private static Example3Component instance = new Example3Component();

	private boolean debug = false;

	public void setDebug(boolean value) {
		debug = value;
	}

	private String derivedProperty1;
	private String derivedProperty2;

	public Example3Component() {
	}

	public Example3Component(@Autowired Properties properties) {
		// NOTE:
		derivedProperty1 = property1;
	}

	@Value("${example.Property1:property1 default value}")
	private String property1;

	@Value("${example.Property2:property2 default value}")
	private String property2;

	public String getProperty1() {
		return property1;
	}

	public String getDerivedProperty1() {
		return derivedProperty1;
	}

	public String getDerivedProperty2() {
		defineDerivedProperties();
		return derivedProperty2;
	}

	public String getProperty2() {
		return property2;
	}

	private void defineDerivedProperties() {
		if (derivedProperty2 == null) {
			derivedProperty2 = property2;
		}
		// NOTE: also set in constructor, which is wrong
		// this code will lead to change of the response page between first and
		// subsequent invocations
		if (derivedProperty1 == null) {
			derivedProperty1 = property1;
		}
	}
}
