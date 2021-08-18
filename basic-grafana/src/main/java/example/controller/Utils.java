package example.controller;
/**
 * Copyright 2021 Serguei Kouzmine
 */

import java.util.List;
import java.util.Map.Entry;

import javax.servlet.http.HttpServletResponse;

import org.springframework.http.HttpHeaders;

public class Utils {

	public static HttpHeaders addResponseHeaders() {
		final HttpHeaders headers = new HttpHeaders();
		headers.add("Access-Control-Allow-Headers", "accept, content-type");
		headers.add("Access-Control-Allow-Methods", "POST");
		headers.add("Access-Control-Allow-Origin", "*");
		return headers;

	}

	public static void addResponseHeaders(final HttpServletResponse response) {
		final HttpHeaders headers = addResponseHeaders();
		for (Entry<String, List<String>> entry : headers.entrySet()) {
			response.setHeader(entry.getKey(), entry.getValue().get(0));
		}
	}
}
