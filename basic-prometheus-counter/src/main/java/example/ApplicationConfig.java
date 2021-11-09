package example;

import javax.ws.rs.ApplicationPath;

import org.glassfish.jersey.server.ResourceConfig;

import org.springframework.stereotype.Component;

import example.controller.Index;
import example.controller.Metrics;

@Component
@ApplicationPath("/")
public class ApplicationConfig extends ResourceConfig {

	public ApplicationConfig() {
		register(Index.class);
		register(Metrics.class);
	}

}
