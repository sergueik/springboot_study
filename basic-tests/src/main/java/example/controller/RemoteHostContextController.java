package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import java.util.HashMap;
import java.util.Map;

// NOTE: not compatible with Java 17+ - will need to be replaced with jakarta.servlet.http
// see also: https://stackoverflow.com/questions/75076727/type-javax-servlet-http-httpservletrequest-not-present-with-gradle-springfox-sw
import javax.servlet.http.HttpServletRequest;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import com.google.gson.Gson;

@RestController
@RequestMapping("/basic")
public class RemoteHostContextController {

	private Map<String, String> data = new HashMap<>();
	private final Gson gson = new Gson();
	private String payload;
	private String remoteHost;

	@GetMapping(value = "/request", produces = {
			MediaType.APPLICATION_JSON_VALUE })

	public ResponseEntity<String> remoteHost2(HttpServletRequest request) {
		remoteHost = request.getRemoteHost();
		data.put("remotehost", remoteHost);
		payload = gson.toJson(data);

		return ResponseEntity.status(HttpStatus.OK).body(payload);
	}

	@GetMapping(value = "/context", produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<String> remoteHost1() {
		RequestAttributes requestAttributes = RequestContextHolder
				.currentRequestAttributes();
		HttpServletRequest httpServletRequest = ((ServletRequestAttributes) requestAttributes)
				.getRequest();
		remoteHost = httpServletRequest.getRemoteHost();
		data = new HashMap<>();
		data.put("remotehost", remoteHost);
		payload = gson.toJson(data);
		return ResponseEntity.status(HttpStatus.OK).body(payload);
	}

}
