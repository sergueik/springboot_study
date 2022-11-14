package example;

import org.apache.http.impl.client.CloseableHttpClient;

import brave.Tracing;
import brave.context.slf4j.MDCCurrentTraceContext;
import brave.http.HttpTracing;
import brave.httpclient.TracingHttpClientBuilder;
import brave.spring.webmvc.TracingHandlerInterceptor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;

import org.springframework.context.annotation.Bean;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;

@RestController
public class BusinessAppController {

	private static final Logger logger = LoggerFactory
			.getLogger(BusinessAppController.class);

	@Bean
	Tracing tracing() {
		return Tracing.newBuilder().localServiceName("business-app")
				.currentTraceContext(MDCCurrentTraceContext.create()).build();
	}

	@Bean
	HttpTracing httpTracing(Tracing tracing) {
		return HttpTracing.create(tracing);
	}

	@Bean
	WebMvcConfigurerAdapter traceIncomingRequests(HttpTracing httpTracing) {
		return new WebMvcConfigurerAdapter() {
			@Override
			public void addInterceptors(InterceptorRegistry registry) {
				registry.addInterceptor(TracingHandlerInterceptor.create(httpTracing));
			}
		};
	}

	/** Makes sure a trace is continued on outgoing requests */
	@Bean
	CloseableHttpClient traceOutgoingRequests(HttpTracing httpTracing) {
		return TracingHttpClientBuilder.create(httpTracing).build();
	}

	@RequestMapping(value = "/", method = RequestMethod.GET)
	public String index() {
		String traceId = MDC.get("traceId");
		String appName = this.getClass().getSimpleName();
		MDC.put("appName", appName);
		logger.info("hello {}", appName);
		return "traceId: " + traceId;
	}

}