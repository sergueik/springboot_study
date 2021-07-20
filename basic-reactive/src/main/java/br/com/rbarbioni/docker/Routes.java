package br.com.rbarbioni.docker;

import br.com.rbarbioni.docker.handler.UserHandler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.MediaType;
import org.springframework.web.reactive.function.server.RouterFunction;

import static org.springframework.web.reactive.function.server.RequestPredicates.*;
import static org.springframework.web.reactive.function.server.RouterFunctions.nest;
import static org.springframework.web.reactive.function.server.RouterFunctions.route;
/**
 * Created by renan on 23/05/17.
 */

@Configuration
public class Routes {

    private static final String PATH = "/api/user";
    private final UserHandler userHandler;

    @Autowired
    public Routes(UserHandler userHandler) {
        this.userHandler = userHandler;
    }

    @Bean
    public RouterFunction<?> routerFunction() {

        return nest(path(PATH),
                nest(accept(MediaType.APPLICATION_JSON),
                        route(GET("/{id}"), userHandler::findById)
                                .andRoute(POST("/"), userHandler::save)
                                .andRoute(PUT("/{id}"), userHandler::update)
                                .andRoute(DELETE("/{id}"), userHandler::delete)
                                .andRoute(GET("/"), userHandler::findAll)
                ));

    }
}
