package com.bookportal.api.auth;

import com.bookportal.api.configs.EnvironmentVariables;
import com.bookportal.api.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.ReactiveUserDetailsService;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

@Service
@RequiredArgsConstructor
public class MyReactiveUserDetailsService implements ReactiveUserDetailsService {
    private final EnvironmentVariables env;
    private final UserRepository userRepository;

    @Override
    public Mono<UserDetails> findByUsername(String username) {
        return userRepository.findByMail(username)
                .switchIfEmpty(Mono.defer(() -> Mono.error(new UsernameNotFoundException(env.userNotFound()))))
                .map(user -> new UserDetails() {
                    @Override
                    public Collection<? extends GrantedAuthority> getAuthorities() {
                        List<GrantedAuthority> authorities = new ArrayList<>();
                        user.getRoles().forEach(role -> authorities.add(new SimpleGrantedAuthority(role.getName())));
                        return authorities;
                    }

                    @Override
                    public String getPassword() {
                        return user.getPassword();
                    }

                    @Override
                    public String getUsername() {
                        return user.getMail();
                    }

                    @Override
                    public boolean isAccountNonExpired() {
                        return user.isActive();
                    }

                    @Override
                    public boolean isAccountNonLocked() {
                        return user.isActive();
                    }

                    @Override
                    public boolean isCredentialsNonExpired() {
                        return user.isActive();
                    }

                    @Override
                    public boolean isEnabled() {
                        return user.isActive();
                    }
                });
    }
}
