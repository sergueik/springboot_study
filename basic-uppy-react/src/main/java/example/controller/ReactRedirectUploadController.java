package example.controller;
/**
 * Copyright 2026 Serguei Kouzmine
 */

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
@RequestMapping("/")
public class ReactRedirectUploadController {

	@Value("${debug}")
	private boolean debug;

    @GetMapping("/react")
    public String redirectToReact() {
        return "redirect:/";
    }

    // alternatively,
    /* 
      @GetMapping("/")
public String redirectToReact() {
    return "forward:/index.html";
}
     */ 
// Spring Boot automatically serves: "src/main/resources/static/index.html" as: "/"
}
