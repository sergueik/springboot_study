package com.findinpath.unleash.web.filter;

import no.finn.unleash.Unleash;
import org.springframework.web.filter.OncePerRequestFilter;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class BlueHeaderFilter extends OncePerRequestFilter {

    private final Unleash unleash;

    public BlueHeaderFilter(Unleash unleash){
        this.unleash = unleash;
    }


    @Override
    protected void doFilterInternal(HttpServletRequest request,
                                    HttpServletResponse response,
                                    FilterChain filterChain) throws ServletException, IOException {

        var blueHeaderEnabled = unleash.isEnabled("blueHeader");
        if (blueHeaderEnabled){
            request.setAttribute("blueHeader", true);
        }

        filterChain.doFilter(request, response);
    }
}
