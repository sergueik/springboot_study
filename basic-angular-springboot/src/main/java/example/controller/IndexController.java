package example.controller;

import javax.servlet.http.HttpServletRequest;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

@Controller
@RequestMapping("/")
public class IndexController {

	@GetMapping
	public String getIndexPage(HttpServletRequest request,
			@RequestParam(value = "description", required = false, defaultValue = "springboot-thymeleaf") String description) {
		request.setAttribute("description", description);
		request.setAttribute("id", "generated id");
		request.setAttribute("app_css_src", "/css/app.css");
		request.setAttribute("app_src", "/js/app.js");
		request.setAttribute("customer_service_src", "/js/service/customer_service.js");
		request.setAttribute("customer_controller_src", "/js/controller/customer_controller.js");

		return "thymeleaf";
	}

	@GetMapping("polling")
	public String getPollingPage(){
		return "polling";
	}

}