package example;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Properties;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.Level;
import org.apache.log4j.PropertyConfigurator;

import org.springframework.stereotype.Component;

@Component
public class LogHelper {

	private static final Logger logger = LogManager.getLogger(LogHelper.class);
	private static final String log4J_properties = String.format("%s/%s/%s",
			System.getProperty("user.dir"), "src/main/resources", "log4j.properties");

	public static Logger getLogger() {
		return logger;
	}

	public LogHelper(/* Class consumerClass */ ) {
		// System.out.println("constructor LogHelper");
		// continue using the legacy log4j configuration file `log4j.xml`

		configureLogger();
		setConfigLocationLogger();

		logger.info("init message");
		logger.warn("init message");
		logger.debug("init message");
	}

	public void info(String data) {
		logger.info("INFO: " + data);
	}

	public void trace(String data) {
		logger.trace("TRACE: " + data);
	}

	public void warn(String data) {
		logger.warn("WARN: " + data);
	}

	public void debug(String data) {
		logger.debug("DEBUG: " + data);
	}

	public void logAll(String data) {
		info(data);
		warn(data);
		debug(data);
	}

	// https://stackoverflow.com/questions/32043770/propertyconfigurator-in-log4j2
	public void configureLogger() {
		Properties logProperties = new Properties();

		try {
			logProperties.load(new FileInputStream(log4J_properties));
			PropertyConfigurator.configure(logProperties);
		} catch (IOException e) {
			throw new RuntimeException("Fail to load: " + log4J_properties);
		}
		logger.info("Configured logger.");
	}

	// alternatively,
	// https://stackoverflow.com/questions/32043770/propertyconfigurator-in-log4j2
	public void setConfigLocationLogger() {
		LoggerContext context = (org.apache.logging.log4j.core.LoggerContext) LogManager
				.getContext(false);
		File file = new File(log4J_properties);
		// this will force a reconfiguration
		context.setConfigLocation(file.toURI());
		logger.info("set ConfigLocation of logger.");
	}
}
