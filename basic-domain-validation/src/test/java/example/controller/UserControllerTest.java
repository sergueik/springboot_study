package example.controller;

import example.model.User;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(UserController.class)
public class UserControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @Test
    void testBlockingValidationFail() throws Exception {
        String invalidUser = "{\"name\":\"\",\"email\":\"invalid-email\"}";
        mockMvc.perform(post("/users/blocking")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(invalidUser))
                .andExpect(status().isBadRequest());
    }

    @Test
    void testCallableValidationFail() throws Exception {
        String invalidUser = "{\"name\":\"\",\"email\":\"invalid-email\"}";
        mockMvc.perform(post("/users/callable")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(invalidUser))
                .andExpect(status().isBadRequest());
    }

    @Test
    void testDeferredValidationFail() throws Exception {
        String invalidUser = "{\"name\":\"\",\"email\":\"invalid-email\"}";
        mockMvc.perform(post("/users/deferred")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(invalidUser))
                .andExpect(status().isBadRequest());
    }
}
