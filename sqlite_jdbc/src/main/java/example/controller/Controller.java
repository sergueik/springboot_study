package example.controller;

import java.util.Iterator;
import java.util.List;
import java.util.Optional;

import javax.annotation.Resource;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import example.entity.Name;
import example.entity.Result;
import example.entity.Student;
import example.service.BaseService;

import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

@RestController
@RequestMapping("/student")
public class Controller {

	@Resource
	private BaseService service;

	@PostMapping("/addStudent")
	public Result addStudent(@RequestBody Student student) {
		return service.addStudent(student);
	}

	@PostMapping("/findAllStudent")
	public Result findAllStudent() {
		return service.findAllStudent();
	}

	@PostMapping("/updateStudent")
	public Result updateStudent(@RequestBody Student student) {
		return service.updateStudent(student);
	}

	@PostMapping("/delStudentById")
	public Result delStudentById(@RequestParam("id") String id) {
		return service.delStudentById(id);
	}

	@PostMapping("/findStudentById")
	public Result findStudentById(@RequestParam("id") String id,
			@RequestParam("cte") Optional<Boolean> cte) {
		return (cte.isPresent() && cte.get()) ? service.findStudentByIdWithCTE(id)
				: service.findStudentById(id);
	}

	@PostMapping("/findStudentByName") // post name inside the body json
	public Result getStudentById(@RequestBody Name studentName) {
		String name = studentName.getName();
		return service.findStudentByName(name);
	}

	// "Content-Type": "application/json"
	@PostMapping("/all")
	// TODO: optional param
	public Result all() {
		return service.all("0");
	}

}
