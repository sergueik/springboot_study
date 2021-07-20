package example.handler;

import example.model.User;
import example.repository.UserRepository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.ValidationUtils;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.reactive.function.server.ServerResponse;
import reactor.core.publisher.Mono;

import javax.validation.Validator;
import java.util.function.Function;

@Service
public class UserHandler {

	@Autowired
	private final UserRepository userRepository;
	private final Validator validator;

	public UserHandler(UserRepository userRepository, Validator validator) {
		this.userRepository = userRepository;
		this.validator = validator;
	}

	public Mono<ServerResponse> findById(ServerRequest serverRequest) {
		return userRepository.findById(serverRequest.pathVariable("id"))
				.flatMap(user -> ServerResponse.ok().body(Mono.just(user), User.class))
				.switchIfEmpty(ServerResponse.notFound().build());
	}

	public Mono<ServerResponse> findAll(ServerRequest serverRequest) {
		return ServerResponse.ok().body(userRepository.findAll(), User.class);
	}

	public Mono<ServerResponse> save(ServerRequest serverRequest) {
		return ServerResponse.ok().body(userRepository.insert(serverRequest.bodyToMono(User.class)), User.class);
	}

	public Mono<ServerResponse> update(ServerRequest serverRequest) {

		return userRepository.findById(serverRequest.pathVariable("id"))
				.flatMap(user -> ServerResponse.ok()
						.body(userRepository.save(serverRequest.bodyToMono(User.class).block()), User.class))
				.switchIfEmpty(ServerResponse.notFound().build());
	}

	public Mono<ServerResponse> delete(ServerRequest serverRequest) {
		return userRepository.findById(serverRequest.pathVariable("id"))
				.flatMap(user -> ServerResponse.noContent().build(userRepository.delete(user)))
				.switchIfEmpty(ServerResponse.notFound().build());
	}

	public <BODY> Mono<ServerResponse> validate(Function<Mono<BODY>, Mono<ServerResponse>> block, ServerRequest request,
			Class<BODY> bodyClass) {
		return request.bodyToMono(bodyClass)
				.flatMap(body -> validator.validate(body).isEmpty() ? block.apply(Mono.just(body))
						: ServerResponse.unprocessableEntity().build());
	}
}