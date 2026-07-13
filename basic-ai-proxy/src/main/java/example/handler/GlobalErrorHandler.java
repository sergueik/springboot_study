package example.handler;

import org.springframework.boot.autoconfigure.web.WebProperties;
import org.springframework.boot.autoconfigure.web.reactive.error.AbstractErrorWebExceptionHandler;
import org.springframework.boot.web.reactive.error.ErrorAttributes;
import org.springframework.context.ApplicationContext;
import org.springframework.core.annotation.Order;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.codec.ServerCodecConfigurer;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.server.*;
import reactor.core.publisher.Mono;

import java.util.Map;

@Component
@Order(-2)
public class GlobalErrorHandler extends AbstractErrorWebExceptionHandler {

	public GlobalErrorHandler(ErrorAttributes errorAttributes, WebProperties webProperties,
			ApplicationContext applicationContext, ServerCodecConfigurer serverCodecConfigurer) {

		super(errorAttributes, webProperties.getResources(), applicationContext);

		this.setMessageReaders(serverCodecConfigurer.getReaders());
		this.setMessageWriters(serverCodecConfigurer.getWriters());

	}

	@Override
	protected RouterFunction<ServerResponse> getRoutingFunction(ErrorAttributes errorAttributes) {
		return RouterFunctions.route(RequestPredicates.all(), this::handleError);
	}

	private Mono<ServerResponse> handleError(ServerRequest request) {

		Throwable error = getError(request);

		HttpStatus status = (error instanceof IllegalArgumentException) ? HttpStatus.NOT_FOUND
				: HttpStatus.INTERNAL_SERVER_ERROR;

		String message = error.getMessage() != null ? error.getMessage() : "Unexpected error";

		return ServerResponse.status(status).contentType(MediaType.APPLICATION_JSON)
				.body(BodyInserters.fromValue(Map.of("error", message)));

	}

}