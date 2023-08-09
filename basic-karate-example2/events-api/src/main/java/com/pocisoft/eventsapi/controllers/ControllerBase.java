package com.pocisoft.eventsapi.controllers;

import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;

import com.pocisoft.eventsapi.security.CustomUserDetails;

public class ControllerBase {

   
	protected int getCurrentUserId() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        CustomUserDetails user= (CustomUserDetails) authentication.getPrincipal();
        return user.getUserId();
    }

}


