package com.spring.stored.procedure.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.spring.stored.procedure.dao.EmployeeDAO;
import com.spring.stored.procedure.model.Employee;

@Service
public class EmployeeServices {

	@Autowired
	private EmployeeDAO dao;

	public List<Employee> employeeList() {
		return dao.employeeList();
	}

	public Employee getEmpById(String emailId) {
		return dao.getEmployee(emailId);
	}
}
