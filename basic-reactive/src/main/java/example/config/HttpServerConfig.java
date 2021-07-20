package example.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;
import org.springframework.data.mongodb.repository.config.EnableReactiveMongoRepositories;
import org.springframework.http.server.reactive.HttpHandler;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.web.reactive.function.server.RouterFunction;
import org.springframework.web.reactive.function.server.RouterFunctions;
import reactor.ipc.netty.http.server.HttpServer;

@Configuration
@EnableAutoConfiguration

// NOTE: the package rename in Eclipse misses updating this property
@EnableReactiveMongoRepositories(basePackages = "example")
public class HttpServerConfig {

	@Autowired
	private Environment environment;

	@Bean
	public HttpServer httpServer(RouterFunction<?> routerFunction) {
		HttpServer server = HttpServer.create();
		server.newHandler(new ReactorHttpHandlerAdapter(RouterFunctions.toHttpHandler(routerFunction)));
		return server;
	}
}