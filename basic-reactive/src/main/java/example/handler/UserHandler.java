package example.handler;

import example.model.User;
import example.repository.UserRepository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.ValidationUtils;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.reactive.function.server.ServerResponse;

import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;

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
		return ServerResponse.ok().body(
				userRepository.insert(serverRequest.bodyToMono(User.class)),
				User.class);
	}

	// modifying - can we keep both ?
/*	
		public Mono<ServerResponse> update(ServerRequest serverRequest) {
	
			return userRepository.findById(serverRequest.pathVariable("id"))
					.flatMap(user -> ServerResponse.ok()
							.body(userRepository.save(serverRequest.bodyToMono(User.class).block()), User.class))
					.switchIfEmpty(ServerResponse.notFound().build());
		}
*/	
	// incorrect signature
	/*
		public Mono<ServerResponse> update(@PathVariable(value = "id") String id,
			@RequestBody User user) {
			return userRepository.findById(id).flatMap(o -> {
				// o.setId(user.getId());
				o.setName(user.getName());
				o.setEmail(user.getEmail());
				
				return userRepository.save(o);
			}).flatMap(o -> ServerResponse.ok().body(Mono.just(o), User.class))
					.switchIfEmpty(ServerResponse.notFound().build());
		}
	*/
// corrected the extra record error
	public Mono<ServerResponse> update(ServerRequest serverRequest) {
		String id = serverRequest.pathVariable("id");
		// NOTE: cannot user block to get the body from serverRequest
		// "Internal Server Error",
		// "message":
		// "block()/blockFirst()/blockLast() are blocking, which is not supported
		// User user = serverRequest.bodyToMono(User.class).block();
		User user = serverRequest.bodyToMono(User.class).toProcessor().peek();
		;

		return userRepository.findById(id).flatMap(o -> {
			// o.setId(user.getId());
			o.setName(user.getName());
			o.setEmail(user.getEmail());
			// not seeing - may have to switch to logger
			System.err.println("Updated values, saving the user: " + o.toString());
			return userRepository.save(o);
		}).flatMap(o -> ServerResponse.ok().body(Mono.just(o), User.class))
				.switchIfEmpty(ServerResponse.notFound().build());
	}

	public Mono<ServerResponse> delete(ServerRequest serverRequest) {
		return userRepository.findById(serverRequest.pathVariable("id"))
				.flatMap(user -> ServerResponse.noContent()
						.build(userRepository.delete(user)))
				.switchIfEmpty(ServerResponse.notFound().build());
	}

	public <BODY> Mono<ServerResponse> validate(
			Function<Mono<BODY>, Mono<ServerResponse>> block, ServerRequest request,
			Class<BODY> bodyClass) {
		return request.bodyToMono(bodyClass)
				.flatMap(body -> validator.validate(body).isEmpty()
						? block.apply(Mono.just(body))
						: ServerResponse.unprocessableEntity().build());
	}
}

