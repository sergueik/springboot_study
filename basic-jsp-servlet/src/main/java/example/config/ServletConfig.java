package example.config;

import org.springframework.boot.web.servlet.ServletRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import example.servlets.SecondServlet;

@Configuration
public class ServletConfig {
	@Bean
	public ServletRegistrationBean getServletRegistrationBean() {
		ServletRegistrationBean bean = new ServletRegistrationBean(
				new SecondServlet());
		bean.addUrlMappings("/second");
		return bean;
	}
}
