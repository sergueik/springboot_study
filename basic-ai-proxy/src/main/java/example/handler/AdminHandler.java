package example.handler;

import java.net.InetSocketAddress;
import java.time.Instant;

import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.reactive.function.server.ServerResponse;

import example.config.AppProperties;
import example.service.IpAllowlistService;
import reactor.core.publisher.Mono;

@Component
public class AdminHandler {
	private static final String ADMIN_TOKEN_HEADER = "X-Admin-Token";
	private final AppProperties appProperties;
	private final IpAllowlistService ipAllowlistService;

	public AdminHandler(AppProperties appProperties, IpAllowlistService ipAllowlistService) {
		this.appProperties = appProperties;
		this.ipAllowlistService = ipAllowlistService;
	}

	public Mono<ServerResponse> allowCurrentIp(ServerRequest request) {
		String configuredToken = appProperties.getSecurity().getAdminToken();
		if (configuredToken == null || configuredToken.isBlank()) {
			return ServerResponse.status(HttpStatus.SERVICE_UNAVAILABLE)
					.bodyValue("{\"error\":\"Admin token is not configured\"}");
		}

		String providedToken = request.headers().firstHeader(ADMIN_TOKEN_HEADER);
		if (!configuredToken.equals(providedToken)) {
			return ServerResponse.status(HttpStatus.FORBIDDEN).bodyValue("{\"error\":\"Invalid admin token\"}");
		}

		InetSocketAddress remoteAddress = request.remoteAddress().orElse(null);
		if (remoteAddress == null || remoteAddress.getAddress() == null) {
			return ServerResponse.status(HttpStatus.BAD_REQUEST).bodyValue("{\"error\":\"Cannot resolve client IP\"}");
		}

		String ip = remoteAddress.getAddress().getHostAddress();
		Instant expiresAt = ipAllowlistService.allow(ip);
		return ServerResponse.ok().bodyValue("{\"allowedIp\":\"" + ip + "\",\"expiresAt\":\"" + expiresAt + "\"}");
	}
}