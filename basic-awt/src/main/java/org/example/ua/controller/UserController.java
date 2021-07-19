package org.example.ua.controller;

import org.example.ua.dto.UserDto;
import org.example.ua.dto.request.LoginRequest;
import org.example.ua.dto.request.RegistrationRequest;
import org.example.ua.dto.response.JwtTokenResponse;
import org.example.ua.exception.NotFoundException;
import org.example.ua.security.UserPrincipal;
import org.example.ua.service.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;
import java.util.List;
import java.util.stream.Collectors;

@RestController
@RequestMapping("user")
public class UserController {

    @Autowired
    private UserService userService;

    @PostMapping("signin")
    public JwtTokenResponse login(@Valid @RequestBody LoginRequest request) {
        return userService.login(request.getEmail(), request.getPassword());
    }

    @PostMapping("register")
    public UserDto register(@Valid @RequestBody RegistrationRequest request) {
        return UserDto.from(userService.register(
                request.getEmail(),
                request.getName(),
                request.getPassword()
        ));
    }

    @PostMapping("token/refresh")
    public JwtTokenResponse refresh(@AuthenticationPrincipal UserPrincipal userPrincipal) {
        return userService.createNewToken(userPrincipal);
    }

    @GetMapping("who")
    public UserDto who(@AuthenticationPrincipal UserPrincipal userPrincipal) {
        return userPrincipal.getUserDto();
    }

    @GetMapping
    @PreAuthorize("hasRole('ADMIN')")
    public List<UserDto> findAll() {
        return userService.findAll()
                .map(UserDto::from)
                .collect(Collectors.toList());
    }

    @PostMapping
    @PreAuthorize("hasRole('ADMIN')")
    public UserDto save(@Valid @RequestBody UserDto userDto) {
        return UserDto.from(userService.save(userDto));
    }

    @PutMapping
    @PreAuthorize("hasRole('ADMIN')")
    public UserDto update(@Valid @RequestBody UserDto userDto) {
        return UserDto.from(userService.update(userDto));
    }

    @DeleteMapping("{email}")
    @PreAuthorize("hasRole('ADMIN')")
    public void delete(@PathVariable String email) {
        userService.delete(email);
    }

    @GetMapping("{email}")
    @PreAuthorize("hasRole('ADMIN')")
    public UserDto findByEmail(@PathVariable String email) {
        return userService.findByEmail(email)
                .map(UserDto::from)
                .orElseThrow(() -> new NotFoundException("User not found"));
    }
}
