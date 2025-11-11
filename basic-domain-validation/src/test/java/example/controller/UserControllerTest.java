package example.controller;


import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import example.model.User;
import example.controller.UserController;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;


import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import com.fasterxml.jackson.databind.ObjectMapper;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.asyncDispatch;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(UserController.class)
public class UserControllerTest {

    @Autowired
    private MockMvc mockMvc;


    @Autowired
    private ObjectMapper objectMapper;
    
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
    @Test
    void testDeferredResultValidUser() throws Exception {
        User validUser = new User("John Doe", "john@example.com");

        MvcResult mvcResult = mockMvc.perform(post("/users/deferred")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(validUser)))
            .andExpect(request().asyncStarted())
            .andReturn();

        // Dispatch async and assert success
        mockMvc.perform(asyncDispatch(mvcResult))
                .andExpect(status().isOk())
                .andExpect(content().string("Created user: John Doe"));
    }

    // ----------------------------
    // Async test: Invalid User
    // ----------------------------
    @Test
    void testDeferredResultInvalidUser() throws Exception {
        User invalidUser = new User("", "bad-email"); // invalid name and email

        MvcResult mvcResult = mockMvc.perform(post("/users/deferred")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(invalidUser)))
            // In SB 3.x, validation may prevent async from starting
            .andReturn();

        // If async never started due to validation, just check 400
        if (mvcResult.getRequest().isAsyncStarted()) {
            mockMvc.perform(asyncDispatch(mvcResult))
                .andExpect(status().isBadRequest());
        } else {
            mockMvc.perform(post("/users/deferred")
                    .contentType(MediaType.APPLICATION_JSON)
                    .content(objectMapper.writeValueAsString(invalidUser)))
                .andExpect(status().isBadRequest());
        }
    }
}

