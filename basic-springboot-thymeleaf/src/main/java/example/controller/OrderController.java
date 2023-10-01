package example.controller;

import java.util.List;
import java.util.Optional;

import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.SessionAttributes;

import example.model.Order;
import example.model.Dish;

// based on: https://qna.habr.com/q/1309576
@Controller
@SessionAttributes("user")
public class OrderController {

	@GetMapping("/order")
	public String getOrder() {
		return "order";
	}

	// java.lang.IllegalStateException: Ambiguous mapping. Cannot map
	// "orderController" method
	// example.controller.OrderController#postOrder(Order)
	// to {POST /order}:
	// There is already "orderController" bean method
	// example.controller.OrderController#postOrder(String, Optional) mapped.
	/*
		@PostMapping("/order")
		public String postOrder(@RequestBody Order order) {
			System.out.println(order.getDishes().size());
			System.out.println(order.getDate());
			System.out.println(order.getName());
			// user.addOrders(order);
			return "order";
		}
	*/

	@RequestMapping(method = RequestMethod.POST, value = "/order", consumes = {
			MediaType.APPLICATION_FORM_URLENCODED_VALUE })

	@PostMapping("/order")
	public String postOrder(@RequestParam String name,
			@RequestParam Optional<List<Dish>> dishes /* , @ModelAttribute User user */) {
		Order order = new Order();
		order.setName(name);
		if (dishes.isPresent()) {
			for (Dish dish : dishes.get()) {
				System.out.println("set dish: " + dish.toString());
				order.SetDish(dish);
			}
		}
		System.out.println(order.getDishes().size());
		System.out.println(order.getDate());
		System.out.println(order.getName());
		// user.addOrders(order);
		return "order";
	}

	/*
	public String postOrder(@RequestParam String name,
			@RequestParam Optional<Dish> burger, @RequestParam Optional<Dish> cola,
			@RequestParam Optional<Dish> chicken, @RequestParam Optional<Dish> tea) {
		Order order = new Order();
		order.setName(name);
		if (burger.isPresent()) {
			System.out.println("set burger");
			order.SetDish(burger.get());
		}
		if (cola.isPresent())
			order.SetDish(cola.get());
		if (chicken.isPresent())
			order.SetDish(chicken.get());
		if (tea.isPresent())
			order.SetDish(tea.get());
		System.out.println(order.getDishes().size());
		System.out.println(order.getDate());
		System.out.println(order.getName());
		// user.addOrders(order);
		return "order";
	}
	*/
	@ModelAttribute
	public void AddDishes(Model model) {
		Dish burger = new Dish("Burger");
		Dish cola = new Dish("Cola");
		Dish chicken = new Dish("Chicken");
		Dish tea = new Dish("Tea");

		model.addAttribute("burger", burger);
		model.addAttribute("cola", cola);
		model.addAttribute("chicken", chicken);
		model.addAttribute("tea", tea);

	}

	@ModelAttribute(name = "order")
	public Order order() {
		return new Order();
	}
	/*
		@ModelAttribute(name = "user")
		public User user() {
			return new User();
		}
	*/
}
