package org.example.ua.service;

import lombok.extern.slf4j.Slf4j;
import org.example.ua.dao.UserRepository;
import org.example.ua.dto.UserDto;
import org.example.ua.dto.response.JwtTokenResponse;
import org.example.ua.entity.User;
import org.example.ua.exception.ValidationResponseException;
import org.example.ua.security.JwtTokenProvider;
import org.example.ua.security.UserPrincipal;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

@Service
@Slf4j
public class UserService {

    @Autowired
    private JwtTokenProvider jwtTokenProvider;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private PasswordEncoder passwordEncoder;


    @Transactional
    public JwtTokenResponse login(String email, String password) {
        userRepository.findByEmail(email)
                .map(User::getPassword)
                .filter(passHash -> passwordEncoder.matches(password, passHash))
                .orElseThrow(() -> new ValidationResponseException("Invalid credentials"));
        return jwtTokenProvider.createToken(email);
    }

    @Transactional
    public User register(String email, String name, String password) {
        userRepository.findByEmail(email)
                .ifPresent(e -> {
                    throw new ValidationResponseException("User already exists");
                });
        String pass = passwordEncoder.encode(password);
        return saveNewUser(email, name, pass);
    }

    private User saveNewUser(String email, String userName, String password) {
        User newUser = new User();
        newUser.setEmail(email);
        newUser.setName(userName);
        newUser.setPassword(password);
        newUser.setRolesList(User.DEFAULT_ROLE);
        userRepository.save(newUser);
        log.debug("Saved new user {}", newUser);
        return newUser;
    }

    public JwtTokenResponse createNewToken(UserPrincipal principal) {
        return jwtTokenProvider.createToken(principal.getUserDto().getEmail());
    }

    @Transactional(readOnly = true)
    public Optional<User> findByEmail(String email) {
        return userRepository.findByEmail(email);
    }

    @Transactional(readOnly = true)
    public Stream<User> findAll() {
        return StreamSupport.stream(userRepository.findAll().spliterator(), false);
    }

    @Transactional
    public void delete(String email) {
        User user = this.findByEmail(email)
                .orElseThrow(() -> new ValidationResponseException("User with email " + email + " does not exists"));
        userRepository.delete(user);
    }

    @Transactional
    public User save(UserDto newUser) {
        User newUserDto = UserDto.to(newUser);
        return userRepository.save(newUserDto);
    }

    @Transactional
    public User update(UserDto userToUpdate) {
        this.findByEmail(userToUpdate.getEmail())
                .orElseThrow(() -> new ValidationResponseException("User with email " + userToUpdate.getEmail() + " does not exists"));
        return userRepository.save(UserDto.to(userToUpdate));
    }
}
