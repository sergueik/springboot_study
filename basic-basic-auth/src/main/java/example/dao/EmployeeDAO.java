package example.dao;

import org.springframework.stereotype.Repository;

import example.model.Employee;
import example.model.Employees;

@Repository
public class EmployeeDAO {
	private static Employees employees = new Employees();

	static {
		employees.getEmployees()
				.add(new Employee(1, "Marc", "McDonald", "marcm@microsoft.com"));
	}

	public Employees getEmployees() {
		return employees;
	}

	public void addEmployee(Employee employee) {
		employees.getEmployees().add(employee);
	}
}
