package example;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
// see: https://www.baeldung.com/spring-enable-config-properties
// omitting this will lead to
// Exception encountered during context initialization -
// cancelling refresh attempt: 
// org.springframework.beans.factory.UnsatisfiedDependencyException: 
// Error creating bean with name 'fileUploadController': 
// Unsatisfied dependency expressed through field 'fileStorageService'; 
// nested exception is org.springframework.beans.factory.UnsatisfiedDependencyException:
// Error creating bean with name 'fileStorageService' defined in file [...FileStorageService.class]:
// Unsatisfied dependency expressed through constructor parameter 0; 
// nested exception is org.springframework.beans.factory.NoSuchBeanDefinitionException:
// No qualifying bean of type 'example.property.FileStorageProperties' 
// available: expected at least 1 bean which qualifies as autowire candidate.
// Dependency annotations: {}
import org.springframework.boot.context.properties.EnableConfigurationProperties;

import example.property.FileStorageProperties;

@SpringBootApplication
@EnableConfigurationProperties({ FileStorageProperties.class })
public class Application {

	public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
	}
}
