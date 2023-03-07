package example.config;

import co.elastic.apm.attach.ElasticApmAttacher;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import javax.annotation.PostConstruct;
import java.util.HashMap;
import java.util.Map;

@Configuration
@ConfigurationProperties(prefix = "elastic.apm")
@ConditionalOnProperty(value = "elastic.apm.enabled", havingValue = "true")
public class ElasticApmConfig {

	@Value("${elastic.apm.server-url:http://apm-server:8200}")
	private String serverUrl;

	@Value("${elastic.apm.service-name:elastic-apm-spring-boot-integration}")
	private String serviceName;

	@Value("${elastic.apm.secret-token}")
	private String secretToken;

	@Value("${elastic.apm.environment}")
	private String environment;

	@Value("${elastic.apm.log-level}")
	private String applicationPackages;

	@Value("${elastic.apm.log-level:INFO}")
	private String logLevel;

	@PostConstruct
	public void init() {

		Map<String, String> apmProps = new HashMap<>();
		apmProps.put("server_url", serverUrl);
		apmProps.put("service_name", serviceName);
		if (secretToken != null && !secretToken.trim().equals("")) {
			apmProps.put("secret_token", secretToken);
		}
		apmProps.put("environment", environment);
		apmProps.put("application_packages", applicationPackages);
		apmProps.put("log_level", logLevel);

		ElasticApmAttacher.attach(apmProps);
	}
}
