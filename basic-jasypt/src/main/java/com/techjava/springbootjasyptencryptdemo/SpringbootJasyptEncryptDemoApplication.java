package com.techjava.springbootjasyptencryptdemo;

import com.ulisesbocchio.jasyptspringboot.annotation.EnableEncryptableProperties;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.stereotype.Component;

@SpringBootApplication
@EnableEncryptableProperties
public class SpringbootJasyptEncryptDemoApplication {

	public static void main(String[] args) {

		ConfigurableApplicationContext configurableApplicationContext = SpringApplication.run(SpringbootJasyptEncryptDemoApplication.class, args);
		MyTest myTest = configurableApplicationContext.getBean(MyTest.class);
		myTest.testPrint();

	}
}

@Component
class MyTest{

	@Value("${username}")
	private String username;

	@Value("${endpoint}")
	private String endpoint;

	public void testPrint(){
		System.out.println("##############################");
		System.out.println("Username is -------->" +username);
		System.out.println("Endpoint is -------->" +endpoint);
		System.out.println("##############################");
	}

}
