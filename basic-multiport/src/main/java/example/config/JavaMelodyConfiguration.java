package example.config;

import javax.servlet.DispatcherType;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import org.springframework.aop.framework.autoproxy.DefaultAdvisorAutoProxyCreator;
import org.springframework.aop.support.annotation.AnnotationMatchingPointcut;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.boot.web.servlet.ServletContextInitializer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Controller;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.RestController;
import net.bull.javamelody.MonitoredWithAnnotationPointcut;
import net.bull.javamelody.MonitoringFilter;
import net.bull.javamelody.MonitoringSpringAdvisor;
import net.bull.javamelody.SessionListener;
import net.bull.javamelody.SpringDataSourceBeanPostProcessor;

@Configuration

public class JavaMelodyConfiguration implements ServletContextInitializer {

	@Override

	public void onStartup(ServletContext servletContext) throws ServletException {
		servletContext.addListener(new SessionListener());
	}

	@Bean(name = "javaMelodyFilter")

	public FilterRegistrationBean javaMelodyFilter() {
		final FilterRegistrationBean javaMelody = new FilterRegistrationBean();
		javaMelody.setFilter(new MonitoringFilter());
		javaMelody.setOrder(1);
		javaMelody.setAsyncSupported(true);
		javaMelody.setName("javaMelody");
		javaMelody.setDispatcherTypes(DispatcherType.REQUEST, DispatcherType.ASYNC);
		javaMelody.addUrlPatterns("/*");
		javaMelody.addInitParameter("monitoring-path", "/monitoring");

		return javaMelody;

	}

	// Note: if you have auto-proxy issues, you can add the following dependency

	// in your pom.xml:

	// <dependency>

	// <groupId>org.aspectj</groupId>

	// <artifactId>aspectjweaver</artifactId>	

	// </dependency>

	@Bean

	public DefaultAdvisorAutoProxyCreator getDefaultAdvisorAutoProxyCreator() {

		return new DefaultAdvisorAutoProxyCreator();

	}

	// Monitoring JDBC datasources

	@Bean

	public SpringDataSourceBeanPostProcessor monitoringDataSourceBeanPostProcessor() {

		final SpringDataSourceBeanPostProcessor processor = new SpringDataSourceBeanPostProcessor();

		processor.setExcludedDatasources(null);

		return processor;

	}

	// Monitoring of beans or methods annotated with @MonitoredWithSpring

	@Bean

	public MonitoringSpringAdvisor monitoringAdvisor() {

		final MonitoringSpringAdvisor interceptor = new MonitoringSpringAdvisor();

		interceptor.setPointcut(new MonitoredWithAnnotationPointcut());

		return interceptor;

	}

	// Monitoring of all services and controllers (even without having

	// @MonitoredWithSpring annotation)

	@Bean

	public MonitoringSpringAdvisor springServiceMonitoringAdvisor() {

		final MonitoringSpringAdvisor interceptor = new MonitoringSpringAdvisor();

		interceptor.setPointcut(new AnnotationMatchingPointcut(Service.class));

		return interceptor;

	}

	@Bean

	public MonitoringSpringAdvisor springControllerMonitoringAdvisor() {

		final MonitoringSpringAdvisor interceptor = new MonitoringSpringAdvisor();

		interceptor.setPointcut(new AnnotationMatchingPointcut(Controller.class));

		return interceptor;

	}

	@Bean

	public MonitoringSpringAdvisor springRestControllerMonitoringAdvisor() {

		final MonitoringSpringAdvisor interceptor = new MonitoringSpringAdvisor();

		interceptor.setPointcut(new AnnotationMatchingPointcut(RestController.class));

		return interceptor;

	}

}
