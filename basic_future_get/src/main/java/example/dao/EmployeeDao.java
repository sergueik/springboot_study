package example.dao;

import java.util.HashMap;
import java.util.Map;

import example.domain.Employee;

public class EmployeeDao {

	private static Map<Integer, Employee> emps;

	static {
		emps = new HashMap<Integer, Employee>();

		emps.put(1001, new Employee(1001, "Bharat", "Mumbai"));
		emps.put(1002, new Employee(1002, "Jayant", "Pune"));
		emps.put(1003, new Employee(1003, "Ravi", "Banglore"));
		emps.put(1004, new Employee(1004, "Kapil", "Delhi"));
		emps.put(1005, new Employee(1005, "Gaurav", "Pune"));
	}

	public static Employee getEmployeeById(int empId) {
		return emps.get(empId);
	}
}
