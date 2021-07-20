package br.com.rbarbioni.docker.handler;

import br.com.rbarbioni.docker.model.User;
import br.com.rbarbioni.docker.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.ValidationUtils;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.reactive.function.server.ServerResponse;
import reactor.core.publisher.Mono;

import javax.validation.Validator;
import java.util.function.Function;

/**
 * Created by renan on 23/05/17.
 */

@Service
public class UserHandler {

    private final UserRepository userRepository;

    private final Validator validator;

    @Autowired
    public UserHandler(UserRepository userRepository, Validator validator) {
        this.userRepository = userRepository;
        this.validator = validator;
    }

    public Mono<ServerResponse> findById(ServerRequest serverRequest){
        return this.userRepository.findById(serverRequest.pathVariable("id"))
                .flatMap(user -> ServerResponse.ok().body(Mono.just(user), User.class))
                .switchIfEmpty(ServerResponse.notFound().build());
    }

    public Mono<ServerResponse> findAll(ServerRequest serverRequest){
        return ServerResponse
                .ok()
                .body(this.userRepository.findAll(), User.class);
    }

    public Mono<ServerResponse> save(ServerRequest serverRequest){
        return ServerResponse
                .ok()
                .body(this.userRepository.insert(serverRequest.bodyToMono(User.class)), User.class);
    }

    public Mono<ServerResponse> update(ServerRequest serverRequest){

        return this.userRepository.findById(serverRequest.pathVariable("id"))
                .flatMap(user -> ServerResponse.ok()
                        .body(this.userRepository.save(serverRequest.bodyToMono(User.class).block()), User.class))
                .switchIfEmpty(ServerResponse.notFound().build());
    }

    public Mono<ServerResponse> delete(ServerRequest serverRequest) {
        return this.userRepository.findById(serverRequest.pathVariable("id"))
                .flatMap(user -> ServerResponse
                        .noContent()
                        .build(this.userRepository.delete(user)))
                .switchIfEmpty(ServerResponse.notFound().build());
    }

    public <BODY> Mono<ServerResponse> validate(Function<Mono<BODY>, Mono<ServerResponse>> block, ServerRequest request, Class<BODY> bodyClass) {
        return request
                .bodyToMono(bodyClass)
                .flatMap(
                        body -> validator.validate(body).isEmpty()
                                ? block.apply(Mono.just(body))
                                : ServerResponse.unprocessableEntity().build());
    }
}