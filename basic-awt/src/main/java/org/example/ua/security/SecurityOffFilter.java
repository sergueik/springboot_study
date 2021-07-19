package org.example.ua.security;

import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.filter.GenericFilterBean;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import java.io.IOException;

//Filter which will permanently authenticate user (for testing)
public class SecurityOffFilter extends GenericFilterBean {

    private final String email;
    private final JwtTokenProvider jwtTokenProvider;

    public SecurityOffFilter(String email, JwtTokenProvider jwtTokenProvider) {
        this.email = email;
        this.jwtTokenProvider = jwtTokenProvider;
    }

    @Override
    public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain) throws IOException, ServletException {
        Authentication auth = jwtTokenProvider.buildAuthentication(email);
        SecurityContextHolder.getContext().setAuthentication(auth);
        filterChain.doFilter(servletRequest, servletResponse);
    }
}
