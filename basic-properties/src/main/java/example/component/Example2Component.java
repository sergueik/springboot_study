package example.component;

import java.util.Properties;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import example.utils.Utils;

@Component
public class Example2Component {

	private Properties properties;
	private static String osName = Utils.getOSName();

	public Example2Component(@Autowired Properties properties) {
		this.properties = properties;
	}

	public String getSomeProperty() {
		if (osName.equals("windows"))
			return (String) properties.get("example.firstProperty");
		else
			return (String) properties.get("example.secondProperty");
	}

}
