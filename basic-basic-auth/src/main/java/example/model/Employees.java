package example.model;

import java.util.ArrayList;
import java.util.List;

public class Employees {
	private List<Employee> employees;

	public List<Employee> getEmployees() {
		if (employees == null) {
			employees = new ArrayList<Employee>();
		}
		return employees;
	}

	public void setEmployees(List<Employee> data) {
		employees = data;
	}
}
