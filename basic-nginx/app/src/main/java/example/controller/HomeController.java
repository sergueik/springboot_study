package example.controller;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Map;

import org.springframework.ui.Model;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Component;
import org.springframework.context.annotation.Bean;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.stereotype.Controller;

import java.time.LocalDateTime;

// origin: https://github.com/kolorobot/spring-boot-thymeleaf
@Controller
@RequestMapping("/")
public class HomeController {
	private String envKey = "APP_SERVER";
	private StringBuffer sb = new StringBuffer();

	@Value("${application}")
	// NOTE: application is a reserved variable name
	private String variable;

	private Log log = LogFactory.getLog(HomeController.class);

	@GetMapping
	String index(Model model) {
		model.addAttribute("now", LocalDateTime.now());
		// https://stackoverflow.com/questions/56102116/access-application-properties-value-in-thymeleaf-template
		model.addAttribute("variable", variable);
		log.info("Setting text from property " + "application" + ":" + variable);
		model.addAttribute("hostname", showHostName());
		log.info("Setting text from environment " + "hostname" + ":" + showHostName());
		return "index";
	}

	public String showHostName() {
		sb = new StringBuffer();
		try {
			String hostname = InetAddress.getLocalHost().getHostName();
			sb.append("Server:" + hostname);
		} catch (UnknownHostException e) {
		}
		return sb.toString();
	}

	public String showEnv() {
		return showEnv(envKey);
	}

	// evaluate specific environment value
	public String showEnv(String key) {
		Map<String, String> map = System.getenv();
		sb.append(key + " = " + System.getenv(key));
		return sb.toString();
	}

}
