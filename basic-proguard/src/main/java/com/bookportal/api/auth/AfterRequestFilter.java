package com.bookportal.api.auth;

import com.bookportal.api.model.RequestLog;
import com.bookportal.api.service.Top20Service;

import lombok.extern.slf4j.Slf4j;
//the lombok.extern.slf4j.Sld4j does not work.
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.stereotype.Component;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilter;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

import java.util.Objects;

@Component
@Slf4j
public class AfterRequestFilter implements WebFilter {
	private final Logger log = LoggerFactory.getLogger(AfterRequestFilter.class);
    @Override
    public Mono<Void> filter(ServerWebExchange exchange, WebFilterChain chain) {
        logRequest(exchange).subscribe();
        return chain.filter(exchange);
    }

    private Mono<Void> logRequest(ServerWebExchange exchange) {
        ServerHttpRequest request = exchange.getRequest();
        exchange.getPrincipal().doOnNext(principal -> log.info("User: {}", principal.getName())).subscribe();

        String parameters = request.getQueryParams().toString();
        String path = request.getPath().toString();
        String requestId = request.getId();
        String uri = request.getURI().toString();
        String remote = "";
        try {
            remote = Objects.requireNonNull(request.getRemoteAddress()).getAddress().getHostAddress();
        } catch (Exception e) {
            log.error("Error while getting remoteAddr");
        }
        RequestLog requestLog = RequestLog.builder()
                .path(path)
                .requestId(requestId)
                .uri(uri)
                .remote(remote)
                .parameters(parameters)
                .build();
        log.info(requestLog.toString());
        return Mono.empty();
    }
}
