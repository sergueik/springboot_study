package br.com.rbarbioni.docker.service;

import br.com.rbarbioni.docker.model.User;
import br.com.rbarbioni.docker.repository.UserRepository;
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
        return this.userRepository.findAll();
    }

    public Mono<User> findById(String id){
        return this.userRepository.findById(id)
                .switchIfEmpty(Mono.error(new HttpServerErrorException(HttpStatus.NOT_FOUND)));
    }

    public Mono<User> save(User data){
        return this.userRepository.save(data);
    }

    public Mono<User> update(String id, User data){
        return this.userRepository.save(data);
    }


    public Mono<Void> delete (String id){
        return this.userRepository.deleteById(id);
    }
}
