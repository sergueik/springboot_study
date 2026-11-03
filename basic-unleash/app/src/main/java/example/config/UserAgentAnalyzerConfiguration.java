package example.config;

import nl.basjes.parse.useragent.UserAgentAnalyzer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class UserAgentAnalyzerConfiguration {

	@Bean
	public UserAgentAnalyzer userAgentAnalyzer() {
		return UserAgentAnalyzer.newBuilder().withField("DeviceClass").build();
	}
}
