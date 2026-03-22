package org.hanbo.boot.app.controllers;

import org.hanbo.boot.app.config.MyJwtTokenUtils;
import org.hanbo.boot.app.models.LoginRequest;
import org.hanbo.boot.app.models.LoginUser;
import org.hanbo.boot.app.models.LoginUserResponse;
import org.hanbo.boot.app.security.MyJwtUserCheckService;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;

@Controller
public class LoginController extends ControllerBase {
	private MyJwtUserCheckService _authService;

	private MyJwtTokenUtils _jwtTknUtils;

	public LoginController(MyJwtUserCheckService authService, MyJwtTokenUtils jwtTknUtils) {
		_authService = authService;
		_jwtTknUtils = jwtTknUtils;
	}

	@RequestMapping(value = "/authenticate", method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<LoginUserResponse> login(@RequestBody LoginRequest loginReq) {
		System.out.println("User Name: " + loginReq.getUserName());
		System.out.println("User Pass: " + loginReq.getUserPass());

		if (!StringUtils.isEmpty(loginReq.getUserName()) && !StringUtils.isEmpty(loginReq.getUserPass())) {
			LoginUser userFound = _authService.authenticateUser(loginReq.getUserName(), loginReq.getUserPass());
			if (userFound != null) {
				String jwtTknVal = _jwtTknUtils.generateToken(userFound);

				if (!StringUtils.isEmpty(jwtTknVal)) {

					LoginUserResponse resp = new LoginUserResponse();

					resp.setActive(userFound.isActive());
					resp.setNickName(userFound.getNickName());
					resp.setUserEmail(userFound.getUserEmail());
					resp.setUserId(userFound.getUserId());
					resp.setUserName(userFound.getUserName());
					resp.setAllUserRoles(userFound.getAllUserRoles());
					resp.setAuthToken(jwtTknVal);

					return ResponseEntity.ok(resp);
				} else {
					return ResponseEntity.status(403).body((LoginUserResponse) null);
				}
			} else {
				return ResponseEntity.status(403).body((LoginUserResponse) null);
			}
		} else {
			return ResponseEntity.status(403).body((LoginUserResponse) null);
		}
	}

	@RequestMapping(value = "/public/authFailed", method = RequestMethod.GET)
	public ModelAndView authFailed() {
		ModelAndView retVal = new ModelAndView();
		retVal.setViewName("authFailedPage");
		return retVal;
	}
}
