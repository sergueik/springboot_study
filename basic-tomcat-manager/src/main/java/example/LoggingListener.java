package example;

import java.io.File;
import java.net.URISyntaxException;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

import org.apache.log4j.Logger;
import org.apache.log4j.xml.DOMConfigurator;

public class LoggingListener implements ServletContextListener {

	@Override
	public void contextInitialized(final ServletContextEvent sce) {
		System.out.println("Initializing the servet context!");
		ServletContext servletContext = sce.getServletContext();
		try {
			String log4jFile = String.format("%s-log4j.xml",
					servletContext.getContextPath());
			System.out.println("Trying to read log4jFile=" + log4jFile);
			String configFilename = new File(
					getClass().getResource(log4jFile).toURI()).getAbsolutePath();
			long delay = 1000;
			System.out.println(
					String.format("Configuring watchdog on log4jFile=%s with delay=%s",
							configFilename, delay));
			DOMConfigurator.configureAndWatch(configFilename, delay);
		} catch (URISyntaxException e) {
			throw new IllegalStateException(
					"Error resolving log4j configuration file for context="
							+ servletContext,
					e);
		}
	}

	@Override
	public void contextDestroyed(final ServletContextEvent sce) {
		System.out.println("Destroying the servet context!");
		Logger.getRootLogger().getLoggerRepository().shutdown();
	}

}
