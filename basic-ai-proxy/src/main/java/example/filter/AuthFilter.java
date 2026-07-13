package example.filter;

import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilter;
import org.springframework.web.server.WebFilterChain;

import example.config.AppProperties;
import example.service.IpAllowlistService;
import reactor.core.publisher.Mono;

@Component
public class AuthFilter implements WebFilter {
    private final AppProperties appProperties;
    private final IpAllowlistService ipAllowlistService;

    public AuthFilter(AppProperties appProperties, IpAllowlistService ipAllowlistService) {
        this.appProperties = appProperties;
        this.ipAllowlistService = ipAllowlistService;
    }

    @Override
    public Mono<Void> filter(ServerWebExchange exchange, WebFilterChain chain) {

        String path = exchange.getRequest().getPath().value();
        if ("/actuator/health".equals(path)) {
            return chain.filter(exchange);
        }
        if ("/admin/allow-ip".equals(path)) {
            return chain.filter(exchange);
        }
        AppProperties.Security security = appProperties.getSecurity();

        if (security == null || !security.isEnabled()) {
            return chain.filter(exchange);
        }
        String remoteAddress = resolveRemoteAddress(exchange);
        if (remoteAddress == null) {
            return forbidden(exchange, "Cannot resolve client IP");
        }
        if (ipAllowlistService.isAllowed(remoteAddress)) {
            return chain.filter(exchange);
        }
        return forbidden(exchange, "IP not allowed: " + remoteAddress);
    }

    private String resolveRemoteAddress(ServerWebExchange exchange) {
        InetSocketAddress remoteAddress = exchange.getRequest().getRemoteAddress();
        if (remoteAddress == null || remoteAddress.getAddress() == null) {
            return null;
        }
        return remoteAddress.getAddress().getHostAddress();
    }

    private Mono<Void> forbidden(ServerWebExchange exchange, String message) {
        exchange.getResponse().setStatusCode(HttpStatus.FORBIDDEN);
        exchange.getResponse().getHeaders().setContentType(MediaType.APPLICATION_JSON);
        byte[] body = ("{\"error\":\"" + message + "\"}").getBytes(StandardCharsets.UTF_8);

        return exchange.getResponse().writeWith(Mono.just(exchange.getResponse().bufferFactory().wrap(body)));

    }
}