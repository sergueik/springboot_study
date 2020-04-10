package example.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

import example.model.Todo;
import example.model.TodoRepository;

import java.util.ArrayList;
import java.util.List;

@Controller
public class DefaultController {
	@Autowired
	TodoRepository todoRepository;

	@RequestMapping("/")
	public String index(Model model) {
		Iterable<Todo> todoIterable = todoRepository.findAll();
		List<Todo> todoList = new ArrayList<>();
		for (Todo todo : todoIterable) {
			todoList.add(todo);
		}
		model.addAttribute("todoList", todoList);
		return "index";
	}
}
