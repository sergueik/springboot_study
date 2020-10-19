package example.config;

import org.springframework.web.servlet.support.AbstractAnnotationConfigDispatcherServletInitializer;

public final class AppInit
		extends AbstractAnnotationConfigDispatcherServletInitializer {

	@Override
	protected final Class<?>[] getRootConfigClasses() {
		return new Class<?>[] { RootConfig.class };
	}

	@Override
	protected final Class<?>[] getServletConfigClasses() {
		return new Class<?>[] { Config.class };
	}

	@Override
	protected final String[] getServletMappings() {
		return new String[] { "/" };
	}

}
