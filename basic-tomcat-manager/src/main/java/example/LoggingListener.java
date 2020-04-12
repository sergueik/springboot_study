package example;

import java.io.File;
import java.net.URISyntaxException;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

import org.apache.logging.log4j.core.Logger;
import org.apache.logging.log4j.LogManager;
// import org.apache.log4j.core.Logger;
// import org.apache.log4j.core.xml.DOMConfigurator;

public class LoggingListener implements ServletContextListener {

	@Override
	public void contextInitialized(final ServletContextEvent sce) {
		System.out.println("Initializing the servet context!");
		ServletContext servletContext = sce.getServletContext();
		try {
			String log4jFile = String.format("%s-log4j.xml", servletContext.getContextPath());
			System.out.println("Trying to read log4jFile=" + log4jFile);
			String configFilename = new File(getClass().getResource(log4jFile).toURI()).getAbsolutePath();
			long delay = 1000;
			// https://logging.apache.org/log4j/2.x/manual/migration.html
			System.out.println(

					String.format("Configuring watchdog on log4jFile=%s with delay=%s", configFilename, delay));
			// TODO:
			// must not configure by calling the classes DOMConfigurator or
			// PropertyConfigurator.
			// DOMConfigurator.configureAndWatch(configFilename, delay);
			// https://stackoverflow.com/questions/32043770/propertyconfigurator-in-log4j2/46199087
		} catch (URISyntaxException e) {
			throw new IllegalStateException("Error resolving log4j configuration file for context=" + servletContext,
					e);
		}
	}

	@Override
	public void contextDestroyed(final ServletContextEvent sce) {
		System.out.println("Destroying the servet context!");
		// TODO: migrate to 2.x
		// LogManager.getRootLogger().getLoggerRepository().shutdown();
	}

}
