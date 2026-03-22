package org.hanbo.boot.app.controllers;

import org.hanbo.boot.app.models.LoginUser;
import org.springframework.security.core.context.SecurityContextHolder;

public class ControllerBase {
	protected LoginUser getCurrentUser() {
		LoginUser retVal = null;
		Object principal = SecurityContextHolder.getContext().getAuthentication().getPrincipal();
		if (principal != null && principal instanceof LoginUser) {
			retVal = (LoginUser) principal;
		}

		return retVal;
	}
}
