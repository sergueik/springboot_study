package example.service;

import example.model.User;
import example.repository.UserRepository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpServerErrorException;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Service
public class UserService {

    private final UserRepository userRepository;

    @Autowired
    public UserService(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    public Flux<User> findAll(){
        return userRepository.findAll();
    }

    public Mono<User> findById(String id){
        return userRepository.findById(id)
                .switchIfEmpty(Mono.error(new HttpServerErrorException(HttpStatus.NOT_FOUND)));
    }

    public Mono<User> save(User data){
        return userRepository.save(data);
    }

    public Mono<User> update(String id, User data){
        return userRepository.save(data);
    }


    public Mono<Void> delete (String id){
        return userRepository.deleteById(id);
    }
}
