package example.dto.response;

import lombok.Data;

@Data
public class JwtTokenResponse {

    private final String token;
    private final long expiresInMilliseconds;
}
