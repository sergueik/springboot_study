
package example;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Import;

import example.bot.ActivityHandlerExample;

//
// This is the starting point of the Sprint Boot Bot application.
// NOTE the heavyweight com.microsoft.bot.integration.spring.BotController dependency
//
@SpringBootApplication

// Using the default BotController to receive incoming Channel messages.
// A custom
// controller could be used by eliminating this import and creating a new
// org.springframework.web.bind.annotation.RestController.
// The default controller is created by the Spring Boot container using
// dependency injection. The default route is /api/messages.
@Import({ com.microsoft.bot.integration.spring.BotController.class })

/**
 * This class extends the BotDependencyConfiguration which provides the default
 * implementations for a Bot application.  The Application class should
 * override methods in order to provide custom implementations.
 */
public class Application
		extends com.microsoft.bot.integration.spring.BotDependencyConfiguration {

	public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
	}

	@Bean
	public com.microsoft.bot.builder.Bot getBot() {
		return new ActivityHandlerExample();
	}

	// Returns a custom "error handling Adapter"
	@Override
	public com.microsoft.bot.integration.BotFrameworkHttpAdapter getBotFrameworkHttpAdaptor(
			com.microsoft.bot.integration.Configuration configuration) {
		return new com.microsoft.bot.integration.AdapterWithErrorHandler(
				configuration);
	}
}
