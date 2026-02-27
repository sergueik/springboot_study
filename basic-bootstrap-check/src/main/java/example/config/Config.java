package example.config;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

// see also:  
// https://codetinkering.com/spring-boot-package-scanning-configuration/
@Configuration
@ComponentScan({ "example.controller", "example.service", "example.model" })
public class Config {

}