package example.service;

import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.server.ServerResponse;

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

		Mono<ResponseEntity<Flux<DataBuffer>>> responseMono = clientRequest.retrieve()
				.onStatus(status -> true, errorResponse -> Mono.empty()).toEntityFlux(DataBuffer.class);

		return responseMono.flatMap(entity -> {
			HttpHeaders filteredHeaders = filterHeaders(entity.getHeaders());
			Flux<DataBuffer> body = entity.getBody() != null ? entity.getBody() : Flux.empty();

			return ServerResponse.status(entity.getStatusCode()).headers(h -> h.addAll(filteredHeaders))
					.body(BodyInserters.fromPublisher(body, DataBuffer.class));
		});
	}

	private boolean hasBody(HttpMethod method) {
		return method != HttpMethod.GET && method != HttpMethod.HEAD;
	}

	private HttpHeaders filterHeaders(HttpHeaders source) {
		HttpHeaders filtered = new HttpHeaders();
		source.forEach((name, values) -> {
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
		source.forEach((name, values) -> {
			if (HttpHeaders.HOST.equalsIgnoreCase(name))
				return;
			if (HttpHeaders.CONTENT_LENGTH.equalsIgnoreCase(name))
				return;
			if (HttpHeaders.ACCEPT_ENCODING.equalsIgnoreCase(name))
				return;
			values.forEach(value -> target.header(name, value));
		});
	}
}