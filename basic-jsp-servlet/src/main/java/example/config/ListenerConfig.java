package example.config;

import org.springframework.boot.web.servlet.ServletListenerRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import example.listener.SecondListener;

import javax.servlet.annotation.WebListener;

@Configuration
public class ListenerConfig {

	@Bean
	public ServletListenerRegistrationBean getServletListenerRegistrationBean() {
		ServletListenerRegistrationBean bean = new ServletListenerRegistrationBean(
				new SecondListener());
		return bean;
	}
}
