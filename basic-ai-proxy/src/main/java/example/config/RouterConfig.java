package example.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.function.server.RouterFunction;
import org.springframework.web.reactive.function.server.ServerResponse;

import example.handler.AdminHandler;
import example.handler.ProxyHandler;

import static org.springframework.web.reactive.function.server.RequestPredicates.POST;
import static org.springframework.web.reactive.function.server.RequestPredicates.all;
import static org.springframework.web.reactive.function.server.RouterFunctions.route;

@Configuration
public class RouterConfig {

    @Bean
    RouterFunction<ServerResponse> routes(ProxyHandler proxyHandler, AdminHandler adminHandler) {
        return route(POST("/admin/allow-ip"), adminHandler::allowCurrentIp).andRoute(all(), proxyHandler::handle);
    }
}