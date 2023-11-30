package example.listener;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

public class SecondListener implements ServletContextListener {
	@Override
	public void contextInitialized(ServletContextEvent sce) {
		System.out.println("Second init.....");
	}

	@Override
	public void contextDestroyed(ServletContextEvent sce) {
		System.out.println("Second Destroyed.....");
	}
}
