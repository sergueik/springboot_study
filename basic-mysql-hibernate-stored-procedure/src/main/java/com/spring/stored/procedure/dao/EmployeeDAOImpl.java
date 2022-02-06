package com.spring.stored.procedure.dao;

import java.util.List;

import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.spring.stored.procedure.model.Employee;

@Repository
public class EmployeeDAOImpl implements EmployeeDAO {
	@Autowired
	private SessionFactory sessionFactory;

	// using direct procedure call
	@SuppressWarnings("unchecked")
	@Override
	public List<Employee> employeeList() {
		return sessionFactory.openSession()
				.createSQLQuery("call getData()")
				.addEntity(Employee.class).list();

	}

	// Procedure call using Named Query
	@Override
	public Employee getEmployee(String emailId) {
		return (Employee) sessionFactory.openSession()
				.getNamedQuery("getEmployeeByEmailId")
				.setParameter("emailId", emailId).uniqueResult();

	}

}
