package example.controller;

import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;

@Controller
public class AppController {

	@GetMapping("/generate")
	@ResponseBody
	public String direct(
			@RequestParam(value = "name", required = true) String name) {
		return "hello " + name;
	}

	@RequestMapping("/model")
	public String jsp(Model model,
			@RequestParam(value = "name", required = false, defaultValue = "World") String name) {
		model.addAttribute("name", name);
		model.addAttribute("id", "0");
		return "hello"; // the name of the JSP model
	}

	@RequestMapping(value = "/model", method = RequestMethod.POST)
	public String model_method(Model model, @RequestParam("id") String id,
			@RequestParam("name") String name) {
		model.addAttribute("name", name);
		model.addAttribute("id", id);
		return "hello"; // the name of the JSP model
	}

	@PostMapping("/model_with_default_param")
	public String model_method_with_default_param(Model model,
			@RequestParam(value = "name", required = false, defaultValue = "World") String name) {
		model.addAttribute("name", name);
		model.addAttribute("id", "0");
		return "hello"; // the name of the JSP model
	}

	// NOTE:
	// without a @ResponseBody annotation
	// and "/json" mapping will be interpreted as
	// javax.servlet.ServletException:
	// Circular view path [json]: would dispatch back to the current handler URL
	// [/json] again.
	// by the test
	// and other mappings e.g. "/generate/json" will be leading to a 404

	@GetMapping(value = "/json", produces = MediaType.APPLICATION_JSON_VALUE)
	@ResponseBody
	public Data json() {
		return new Data("hello");
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
