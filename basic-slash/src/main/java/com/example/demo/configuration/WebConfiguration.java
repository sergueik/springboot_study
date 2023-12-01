package com.example.demo.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.PathMatchConfigurer;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
public class WebConfiguration implements WebMvcConfigurer {

	@Value("${slash:false}")
	private boolean slash;

	@SuppressWarnings("deprecation")
	@Override
	public void configurePathMatch(PathMatchConfigurer configurer) {
		configurer.setUseTrailingSlashMatch(slash);
	}
}