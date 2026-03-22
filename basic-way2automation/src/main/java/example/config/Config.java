package example.config;

import java.util.Arrays;
import java.util.List;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.format.FormatterRegistry;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.validation.MessageCodesResolver;
import org.springframework.validation.Validator;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.method.support.HandlerMethodReturnValueHandler;
import org.springframework.web.servlet.HandlerExceptionResolver;
import org.springframework.web.servlet.config.annotation.AsyncSupportConfigurer;
import org.springframework.web.servlet.config.annotation.ContentNegotiationConfigurer;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.DefaultServletHandlerConfigurer;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.PathMatchConfigurer;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.ViewControllerRegistry;
import org.springframework.web.servlet.config.annotation.ViewResolverRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
@EnableWebMvc
public class Config implements WebMvcConfigurer {

	// optional: adding the default locations
	// NOTE: every path has to be mapped: non-mapped paths will be 404'ed
	// see the tests
	@Override
	public void addResourceHandlers(ResourceHandlerRegistry registry) {

		for (String dir : Arrays.asList("images", "css", "js")) {
			registry.addResourceHandler(String.format("/%s/**", dir)).addResourceLocations(
					String.format("classpath:/static/%s/", dir),
					String.format("file:///%s/src/main/resources/static/%s/",
							System.getProperty("user.dir").replaceAll("\\\\", "/"), dir));

			registry.addResourceHandler("/*").addResourceLocations("classpath:/static", String.format(
					"file:///%s/src/main/resources/static/", System.getProperty("user.dir").replaceAll("\\\\", "/")));

		}
	}

	@Override
	public void addArgumentResolvers(List<HandlerMethodArgumentResolver> arg) {
	}

	@Override
	public void addCorsMappings(CorsRegistry arg) {

	}

	@Override
	public void addFormatters(FormatterRegistry arg) {

	}

	@Override
	public void addInterceptors(InterceptorRegistry arg) {
	}

	@Override
	public void addReturnValueHandlers(List<HandlerMethodReturnValueHandler> arg) {
	}

	@Override
	public void addViewControllers(ViewControllerRegistry arg) {
	}

	@Override
	public void configureAsyncSupport(AsyncSupportConfigurer arg) {

	}

	@Override
	public void configureContentNegotiation(ContentNegotiationConfigurer arg) {
	}

	@Override
	public void configureDefaultServletHandling(DefaultServletHandlerConfigurer arg) {
	}

	@Override
	public void configureHandlerExceptionResolvers(List<HandlerExceptionResolver> arg) {
	}

	@Override
	public void configureMessageConverters(List<HttpMessageConverter<?>> arg) {
	}

	@Override
	public void configurePathMatch(PathMatchConfigurer arg) {
	}

	@Override
	public void configureViewResolvers(ViewResolverRegistry arg) {
	}

	@Override
	public void extendHandlerExceptionResolvers(List<HandlerExceptionResolver> arg) {
	}

	@Override
	public void extendMessageConverters(List<HttpMessageConverter<?>> arg) {
	}

	@Override
	public MessageCodesResolver getMessageCodesResolver() {
		return null;
	}

	@Override
	public Validator getValidator() {
		return null;
	}

}
