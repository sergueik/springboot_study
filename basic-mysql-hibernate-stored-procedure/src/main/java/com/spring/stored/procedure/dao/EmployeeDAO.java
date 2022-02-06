package com.spring.stored.procedure.dao;

import java.util.List;

import com.spring.stored.procedure.model.Employee;

public interface EmployeeDAO {

	public List<Employee> employeeList();

	public Employee getEmployee(String emailId);

}
