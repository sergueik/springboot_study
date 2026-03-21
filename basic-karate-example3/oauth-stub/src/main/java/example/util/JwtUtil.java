package example.util;


import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class JwtUtil {

    public boolean validate(String token) {
        // replace with your real JWT validation logic
        return token != null && !token.isEmpty();
    }

    public String getUsername(String token) {
        // replace with real parsing of JWT claims
        return "test";
    }

    public List<String> getRoles(String token) {
        // replace with real roles extracted from JWT
        return List.of("ROLE_USER");
    }
}