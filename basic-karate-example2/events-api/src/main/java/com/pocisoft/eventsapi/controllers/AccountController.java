package com.pocisoft.eventsapi.controllers;

import com.pocisoft.eventsapi.security.CustomUserDetails;
import com.pocisoft.eventsapi.security.JwtRequest;
import com.pocisoft.eventsapi.security.JwtResponse;
import com.pocisoft.eventsapi.security.JwtTokenUtil;
import com.pocisoft.eventsapi.security.JwtUserDetailsService;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.DisabledException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.userdetails.UserDetails;
@RestController
@RequestMapping("/api")
public class AccountController {

	  private final AuthenticationManager authenticationManager;
	    private final JwtTokenUtil jwtTokenUtil;
	    private final JwtUserDetailsService userDetailsService;

	    public AccountController(
	            AuthenticationManager authenticationManager,
	            JwtTokenUtil jwtTokenUtil,
	            JwtUserDetailsService userDetailsService
	    ) {
	        this.authenticationManager = authenticationManager;
	        this.jwtTokenUtil = jwtTokenUtil;
	        this.userDetailsService = userDetailsService;
	    }

    @PostMapping("/token")
    public ResponseEntity<JwtResponse> generateToken(@RequestBody JwtRequest authenticationRequest) throws Exception {
        authenticate(authenticationRequest.getUsername(), authenticationRequest.getPassword());

        final CustomUserDetails  userDetails = (CustomUserDetails) userDetailsService.loadUserByUsername(authenticationRequest.getUsername());

        final String token = jwtTokenUtil.generateToken(userDetails);

        return ResponseEntity.ok(new JwtResponse(token));
    }

    private void authenticate(String username, String password) throws Exception {
        try {
            authenticationManager.authenticate(new UsernamePasswordAuthenticationToken(username, password));
        } catch (DisabledException e) {
            throw new Exception("USER_DISABLED", e);
        } catch (BadCredentialsException e) {
            throw new Exception("INVALID_CREDENTIALS", e);
        }
    }

    // Add other account-related methods if needed
}
