package example.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import example.dto.AuthRequest;
import example.service.AuthService;

import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@RestController
public class AuthController {

	private static final Logger log = LoggerFactory.getLogger(AuthController.class);

	@Autowired
	private AuthService authService;

	@Value("${example.username}")
	private String username;

	@Value("${example.password}")
	private String password;

	@RequestMapping(method = RequestMethod.POST, value = "/token", produces = { MediaType.APPLICATION_JSON_VALUE })

	public ResponseEntity<?> token(@RequestBody AuthRequest request) {

		log.info("token request for {}/{}", request.username, request.password);
		// 🔐 STUB authentication against DB / LDAP / IdP
		if (!username.equals(request.username) || !password.equals(request.password)) {
			return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body("Invalid credentials");
		}

		String response = authService.generateToken(request.username);

		return ResponseEntity.ok(Map.of("access_token", response, "token_type", "Bearer", "expires_in", 3600));
	}
}
