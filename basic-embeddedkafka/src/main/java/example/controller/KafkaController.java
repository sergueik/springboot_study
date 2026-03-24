package example.controller;

import example.service.KafkaProducerService;
import org.springframework.stereotype.Controller;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseCookie;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;


@RestController
@RequestMapping("/kafka")
public class KafkaController {

	@Autowired
	private final KafkaProducerService producer;

	public KafkaController(KafkaProducerService producer) {
		this.producer = producer;
	}

	@PostMapping("/send")
	public String send(@RequestParam String msg) {
		producer.send("your-topic", msg);
		return "sent";
	}
}