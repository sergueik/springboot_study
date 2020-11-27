package example;

import javafx.application.Application;
import org.springframework.boot.SpringApplication;
import org.springframework.context.ConfigurableApplicationContext;

// cloned from https://github.com/ruslanys/sample-spring-boot-javafx/blob/master/src/main/java/ru/habrahabr/AbstractJavaFxApplicationSupport.java

public abstract class AbstractJavaFxApplicationSupport extends Application {

	private static String[] savedArgs;
	protected ConfigurableApplicationContext context;

	@Override
	public void init() throws Exception {
		context = SpringApplication.run(getClass(), savedArgs);
		context.getAutowireCapableBeanFactory().autowireBean(this);
	}

	@Override
	public void stop() throws Exception {
		super.stop();
		context.close();
	}

	@SuppressWarnings("restriction")
	protected static void launchApp(
			Class<? extends AbstractJavaFxApplicationSupport> appClass,
			String[] args) {
		AbstractJavaFxApplicationSupport.savedArgs = args;
		Application.launch(appClass, args);
	}
}
