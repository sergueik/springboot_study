package example.controller;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/cast")
public class CastController {

	// passing class properties through form inputs
	@PostMapping(value = "/data", consumes = { MediaType.APPLICATION_FORM_URLENCODED_VALUE }, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	@ResponseBody
	public String getForm(Data data) {
		System.out.println(data);
		return data.toString();

	}

	// based on curious novice question
	// https://qna.habr.com/q/1345210
	public static class Data {

		private int value;
		private String name;

		public String getName() {
			return name;
		}

		public void setName(String data) {
			name = data;
		}

		public void setValue(int data) {
			value = data;
		}

		public int getValue() {
			return value;
		}

		public Data(int value, String name) {
			this.value = value;
			this.name = name;
		}

		@Override
		public String toString() {
			return String.format("{name: \"%s\" value: %s}", this.name, String.valueOf(this.value));
		}

		// NOTE: when a no-arg constructor is absent get a runtime error:
		public Data() {
		}

		/*
		 * 
		 * unexpected error (type=Internal Server Error, status=500). Failed to
		 * instantiate [example.controller.CastController$Data]: No default
		 * constructor found; nested exception is
		 * java.lang.NoSuchMethodException:
		 * example.controller.CastController$Data.<init>() at
		 * java.lang.Class.getConstructor0(Class.java:3082)
		 * java.lang.Class.getDeclaredConstructor(Class.java:2178)
		 */

	}

}
