package example.controller;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.context.config.annotation.RefreshScope;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

@RefreshScope
@Controller
public class GreetingController {

	@Value("${greeting}")
	String greeting;

	@RequestMapping("/greeting")
	public String getGreeting(Model m) {

		m.addAttribute("greeting", greeting);

		// name of view
		return "rateview";

	}

}
