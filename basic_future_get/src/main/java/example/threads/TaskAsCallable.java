package example.threads;

import java.util.concurrent.Callable;

import example.dao.EmployeeDao;
import example.domain.Employee;

public class TaskAsCallable implements Callable<Employee> {

	private int empId;

	public TaskAsCallable(int empId) {
		this.empId = empId;
	}

	public Employee call() throws Exception {

		return EmployeeDao.getEmployeeById(this.empId);
	}
}
