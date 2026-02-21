package example.controller;

import example.service.OrderService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/orders")
public class OrderController {

	private final OrderService service;

	public OrderController(OrderService service) {
		this.service = service;
	}

	@PostMapping
	public ResponseEntity<Void> placeOrder(@RequestParam int quantity) {
		service.placeOrder(quantity);
		return ResponseEntity.ok().build();
	}
}