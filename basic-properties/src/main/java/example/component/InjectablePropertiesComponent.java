package example.component;

import java.util.Properties;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class InjectablePropertiesComponent {

	private Properties properties;

	public InjectablePropertiesComponent(@Autowired Properties properties) {
		this.properties = properties;
	}

	protected static String osName = getOSName();

	public String getSomeProperty() {
		if (getOSName().equals("windows"))
			return (String) properties.get("example.firstProperty");
		else
			return (String) properties.get("example.secondProperty");
	}

	public static String getOSName() {
		if (osName == null) {
			osName = System.getProperty("os.name").toLowerCase();
			if (osName.startsWith("windows")) {
				osName = "windows";
			}
		}
		return osName;
	}

}
