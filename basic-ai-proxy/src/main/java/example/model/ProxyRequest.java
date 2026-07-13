package example.model;

import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.util.MultiValueMap;
import reactor.core.publisher.Flux;
import example.config.ProviderProperties;

public record ProxyRequest(ProviderProperties provider, HttpMethod method, String path, HttpHeaders headers,
                           MultiValueMap<String, String> query, Flux<DataBuffer> body) {
}