package example.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.filter.CommonsRequestLoggingFilter;

@Configuration
public class RequestLoggingConfig {

	// CommonsRequestLoggingFilter is Spring built-in request logger based on
	// AbstractRequestLoggingFilter
	@Bean
	public CommonsRequestLoggingFilter requestLoggingFilter() {
		CommonsRequestLoggingFilter filter = new CommonsRequestLoggingFilter();

		filter.setIncludeClientInfo(true);
		filter.setIncludeQueryString(true);
		filter.setIncludeHeaders(true);
		// filter.setIncludePayload(false); // IMPORTANT for large uploads
		filter.setIncludePayload(true); // for debugging
		filter.setMaxPayloadLength(1024);

		return filter;
	}
}
