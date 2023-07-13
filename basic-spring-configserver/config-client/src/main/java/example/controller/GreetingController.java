package example.controller;

import java.util.HashMap;
import java.util.Map;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.context.config.annotation.RefreshScope;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

@RefreshScope
@Controller
public class GreetingController {

	// defaut value is required to prevent the app compilation errors when config
	// server is down
	@Value("${greeting:dummy}")
	String greeting;
	@Value("${spring.config.import}")
	String value;

	@RequestMapping("/greeting")
	public String getGreeting(Model model) {
		Map<String, String> config = new HashMap<>();
		config.put("import", value);
		Map<String, Map> springconfig = new HashMap<>();
		springconfig.put("config", config);
		model.addAttribute("greeting", greeting);
		// NOTE: only plain variable names for strring values.
		// the assignment
		// model.addAttribute("spring.config.import", config);
		// leads to error in runtime
		// org.thymeleaf.exceptions.TemplateProcessingException:
		// Exception evaluating SpringEL expression:
		// "spring.config.import" (template: "greeting" - line 11, col 9)]
		// with the root cause
		// org.springframework.expression.spel.SpelEvaluationException:
		// EL1007E: Property or field 'config' cannot be found on null
		// 
		// model.addAttribute("spring", springconfig);
		return "greeting";

	}

}
