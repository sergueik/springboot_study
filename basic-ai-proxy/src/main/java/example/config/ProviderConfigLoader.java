package example.config;

import java.util.LinkedHashMap;
import java.util.Map;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

@Component
public class ProviderConfigLoader implements ApplicationRunner {

	private final AppProperties appProperties;

	private final ObjectMapper objectMapper;

	public ProviderConfigLoader(AppProperties appProperties, ObjectMapper objectMapper) {
		this.appProperties = appProperties;
		this.objectMapper = objectMapper;
	}

	@Override
	public void run(ApplicationArguments args) throws Exception {

		String json = appProperties.getProvidersJson();

		if (json == null || json.isBlank()) {
			return;
		}
		// construct simplified MapType instance for LinkedHashMap<String, Map>
		Map<String, Map<String, String>> raw = objectMapper.readValue(json,
				objectMapper.getTypeFactory().constructMapType(LinkedHashMap.class, String.class, Map.class));

		// convert to Map<String, ProviderProperties>
		Map<String, ProviderProperties> providers = new LinkedHashMap<>();

		raw.forEach((String name, Map<String, String> fields) -> {
			ProviderProperties provider = new ProviderProperties();
			provider.setBaseUrl(fields.get("baseUrl"));

			providers.put(name, provider);

		});

		appProperties.setProviders(providers);

	}

}