package example.controller;

import example.model.User;
import example.repository.UserRepository;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.WebFluxTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.reactive.server.WebTestClient;
import reactor.core.publisher.Mono;

import static org.mockito.Mockito.when;

@WebFluxTest(UserReactiveController.class)
class UserReactiveControllerTest {

    @Autowired
    private WebTestClient client;

    @MockBean
    private UserRepository repository;

    @Test
    void testCreateUser() {
        User user = new User("John Doe", "john@example.com");

	// fix thr test Mockito argument evaluation - equality is too strict
        // when(repository.save(user)).thenReturn(Mono.just(user));
        when(repository.save(org.mockito.ArgumentMatchers.any(User.class)))
        .thenReturn(Mono.just(user));

        client.post().uri("/users/reactive")
                .bodyValue(user)
                .exchange()
                .expectStatus().isCreated()
                .expectBody()
                .jsonPath("$.name").isEqualTo("John Doe");
    }
}

