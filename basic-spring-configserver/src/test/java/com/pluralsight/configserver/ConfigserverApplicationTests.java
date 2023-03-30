package com.pluralsight.configserver;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
class ConfigserverApplicationTests {

	// @Disabled
	// NOTE: with 
//	 NOTE:  the spring-boot-starter-parent      2.3.4 release date is Sep 17, 2020. The  spring-cloud-dependencies » 2020.0.0 is        Dec 22, 2020. not working
 //  try
// Hoxton.SR8
	//09:09:37.759 [main] ERROR org.springframework.boot.SpringApplication - Application run failed
//	java.lang.NoClassDefFoundError: org/springframework/boot/Bootstrapper
	// https://stackoverflow.com/questions/74658355/how-to-fix-java-lang-noclassdeffounderror-org-springframework-boot-bootstrapper
	@Test
	void contextLoads() {
	}

}
