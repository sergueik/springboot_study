package example.service;

import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpMethod;
// NOTE: The class introduced in Spring Framework 6.x / Spring Boot 3.x
import org.springframework.http.HttpStatusCode;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.server.ServerResponse;
import java.util.List;
import java.util.function.Predicate;
import java.util.function.Function;

import example.model.ProxyRequest;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.net.URI;

@Service
public class ProxyService {
	private final WebClient webClient;
	private final UrlBuilderService urlBuilderService;

	public ProxyService(WebClient webClient, UrlBuilderService urlBuilderService) {
		this.webClient = webClient;
		this.urlBuilderService = urlBuilderService;
	}

	public Mono<ServerResponse> forward(ProxyRequest request) {
		URI uri = urlBuilderService.build(request);

		WebClient.RequestBodySpec requestBodySpec = webClient.method(request.method()).uri(uri);
		copyHeaders(request.headers(), requestBodySpec);

		WebClient.RequestHeadersSpec<?> clientRequest = hasBody(request.method())
				? requestBodySpec.body(request.body(), DataBuffer.class)
				: requestBodySpec;

		// NOTE: The signature depends on target Spring version
		// The compilation failure incompatible types: incompatible parameter types in lambda expression

		// Spring Boot 2.x:
		/*		
		Mono<ResponseEntity<Flux<DataBuffer>>> responseMono = clientRequest.retrieve()
				.onStatus((HttpStatus status) -> true, (ClientResponse errorResponse) -> Mono.empty())
				.toEntityFlux(DataBuffer.class);
		*/ 
				
		// Spring Boot 3.x
		Mono<ResponseEntity<Flux<DataBuffer>>> responseMono = clientRequest.retrieve()
				.onStatus((HttpStatusCode status) -> true, (ClientResponse errorResponse) -> Mono.empty())
				.toEntityFlux(DataBuffer.class);

		return responseMono.flatMap((ResponseEntity<Flux<DataBuffer>> entity) -> {
			HttpHeaders filteredHeaders = filterHeaders(entity.getHeaders());
			Flux<DataBuffer> body = entity.getBody() != null ? entity.getBody() : Flux.empty();

			return ServerResponse.status(entity.getStatusCode())
					.headers((HttpHeaders headers) -> headers.addAll(filteredHeaders))
					.body(BodyInserters.fromPublisher(body, DataBuffer.class));
		});
	}

	private boolean hasBody(HttpMethod method) {
		return method != HttpMethod.GET && method != HttpMethod.HEAD;
	}

	private HttpHeaders filterHeaders(HttpHeaders source) {
		HttpHeaders filtered = new HttpHeaders();
		source.forEach((String name, List<String> values) -> {
			if (HttpHeaders.CONTENT_LENGTH.equalsIgnoreCase(name))
				return;
			if (HttpHeaders.TRANSFER_ENCODING.equalsIgnoreCase(name))
				return;
			if (HttpHeaders.CONTENT_ENCODING.equalsIgnoreCase(name))
				return;
			filtered.put(name, values);
		});
		return filtered;
	}

	private void copyHeaders(HttpHeaders source, WebClient.RequestBodySpec target) {
		source.forEach((String name, List<String> values) -> {
			if (HttpHeaders.HOST.equalsIgnoreCase(name))
				return;
			if (HttpHeaders.CONTENT_LENGTH.equalsIgnoreCase(name))
				return;
			if (HttpHeaders.ACCEPT_ENCODING.equalsIgnoreCase(name))
				return;
			values.forEach((String value) -> target.header(name, value));
		});
	}
}