package example.handler;

import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.reactive.function.server.ServerResponse;

import example.config.ProviderProperties;
import example.model.ProviderPath;
import example.model.ProxyRequest;
import example.service.ProviderService;
import example.service.ProxyService;
import reactor.core.publisher.Mono;

@Component
public class ProxyHandler {
    private final ProviderService providerService;
    private final ProxyService proxyService;

    public ProxyHandler(ProviderService providerService, ProxyService proxyService) {
        this.providerService = providerService;
        this.proxyService = proxyService;
    }

    public Mono<ServerResponse> handle(ServerRequest request) {
        ProviderPath providerPath = ProviderPath.parse(request.path());
        ProviderProperties provider = providerService.getProvider(providerPath.provider());
        ProxyRequest proxyRequest = new ProxyRequest(provider, request.method(), providerPath.path(), request.headers().asHttpHeaders(), request.queryParams(), request.bodyToFlux(DataBuffer.class));
        return proxyService.forward(proxyRequest);
    }
}