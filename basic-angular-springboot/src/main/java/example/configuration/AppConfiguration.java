package example.configuration;

import java.util.Arrays;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.ViewResolverRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;
import org.springframework.web.servlet.view.InternalResourceViewResolver;

@Configuration
@EnableWebMvc
@ComponentScan(basePackages = "example")
public class AppConfiguration extends WebMvcConfigurerAdapter {

	private Logger logger = LoggerFactory.getLogger(this.getClass());

	@Override
	public void configureViewResolvers(ViewResolverRegistry registry) {
		InternalResourceViewResolver viewResolver = new InternalResourceViewResolver();
		viewResolver.setPrefix("/templates/");
		viewResolver.setSuffix(".jsp");
		registry.viewResolver(viewResolver);
	}

	// Adding the default locations
	// NOTE: every path has tobe mapped: non-mapped paths will be 404'ed
	// see the tests
	@Override
	public void addResourceHandlers(ResourceHandlerRegistry registry) {

		for (String dir : Arrays.asList("images", "css", "js")) {
			logger.info(String.format("Mapped resource handler \"%s\", \"%s\" to \"%s\"",
					String.format("classpath:/static/%s/", dir), System.getProperty("user.dir").replaceAll("\\\\", "/"),
					dir), dir);

			registry.addResourceHandler(String.format("/%s/**", dir)).addResourceLocations(
					String.format("classpath:/static/%s/", dir),
					String.format("file:///%s/src/main/resources/static/%s/",
							System.getProperty("user.dir").replaceAll("\\\\", "/"), dir));

		}
	}
}