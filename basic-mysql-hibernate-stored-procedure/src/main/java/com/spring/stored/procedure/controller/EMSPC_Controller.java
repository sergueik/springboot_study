package com.spring.stored.procedure.controller;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import com.spring.stored.procedure.model.Employee;
import com.spring.stored.procedure.service.EmployeeServices;

@Controller
public class EMSPC_Controller {

	@Autowired(required = true)
	private EmployeeServices services;

	@RequestMapping(value = "/getAllEmployees")
	public String getAllEmployees(Model model) {
		List<Employee> employees = services.employeeList();
		model.addAttribute("employees", employees);
		return "employee";
	}

	@RequestMapping(value = "/getEmployees")
	public String getEmployeeByEmail(@RequestParam("email") String email,
			Model model) {
		List<Employee> employees = new ArrayList<>();
		Employee employee = services.getEmpById(email);
		employees.add(employee);
		model.addAttribute("employees", employees);
		return "employee";
	}
}
