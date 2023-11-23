package example.controller;
/**
 * Copyright 2021,2022,2023 Serguei Kouzmine
 */

import java.util.HashMap;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import example.service.ExampleService;

@RestController
@RequestMapping("/typed")
public class TypedController {

	private static Gson gson = new GsonBuilder().create();
	private boolean debug = false;

	@PostMapping(value = "/data", consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = {
					MediaType.APPLICATION_JSON_VALUE })
	public String data(@RequestBody Data data) {
		return gson.toJson(data);
	}

	@PostMapping(value = "/map", consumes = {
			MediaType.APPLICATION_JSON_VALUE }, produces = {
					MediaType.APPLICATION_JSON_VALUE })
	public String mapdata(@RequestBody Map<String, String> data) {
		return gson.toJson(data);
	}

	public static class Data {

		private boolean status;
		private String name;

		public boolean isStatus() {
			return status;
		}

		public void setStatus(boolean status) {
			this.status = status;
		}

		public String getName() {
			return name;
		}

		public void setName(String data) {
			name = data;
		}

		public Data(String name) {
			this.name = name;
		}

		public Data() {
		}

		@Override
		public String toString() {

			return "Data {" + "name=" + this.name + '}';
		}
	}
}
