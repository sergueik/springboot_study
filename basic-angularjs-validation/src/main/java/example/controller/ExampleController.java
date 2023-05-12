package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */
import org.springframework.web.bind.annotation.PathVariable;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.http.ResponseEntity;
import org.springframework.http.MediaType;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;

import org.springframework.web.servlet.ModelAndView;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

@Controller
public class ExampleController {


	private Log log = LogFactory.getLog(this.getClass());

	@GetMapping(value = "/json/{name}", produces = { MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<Data> json(@PathVariable("name") String name) {
		
		return ResponseEntity.status(HttpStatus.OK)
				.body(new Data(name));

	}


	@GetMapping("/home")
	public ModelAndView getHomePage() {
		log.info("Setting home page");
		ModelAndView modelAndView = new ModelAndView("home");

		return modelAndView;
	}
	@ResponseBody
	@GetMapping("/page")
	public String getPage() {
		return "page is here";
	}
	public static class Data {

		private String name;

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
	}
}
