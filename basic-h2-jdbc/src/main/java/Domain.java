import entity.Address;
import entity.EmplProj;
import entity.Employee;
import entity.Project;
import service.AddressService;
import service.EmplProjService;
import service.EmployeeService;
import service.ProjectService;

import java.sql.SQLException;
import java.util.Calendar;
import java.util.List;

public class Domain {

	public static void main(String[] args) {
		AddressService addressService = new AddressService();
		EmployeeService employeeService = new EmployeeService();
		ProjectService projectService = new ProjectService();
		EmplProjService emplProjService = new EmplProjService();

		Address address = new Address();
		address.setId(1L);
		address.setCountry("DC");
		address.setCity("Gotham City");
		address.setStreet("Arkham street 1");
		address.setPostCode("12345");

		Employee employee = new Employee();
		employee.setId(1L);
		employee.setFirstName("James");
		employee.setLastName("Gordon");

		Calendar calendar = Calendar.getInstance();
		calendar.set(1939, Calendar.MAY, 1);

		employee.setBirthday(new java.sql.Date(calendar.getTime().getTime()));
		employee.setAddressId(address.getId());

		Project project = new Project();
		project.setId(1L);
		project.setTitle("Gotham City Police Department Commissioner");

		EmplProj emplProj = new EmplProj();
		emplProj.setEmployeeId(employee.getId());
		emplProj.setProjectId(project.getId());

		try {
			// addressService.add(address);
			// employeeService.add(employee);
			// projectService.add(project);
			// emplProjService.add(emplProj);

			List<Address> addressList = addressService.getAll();
			List<Employee> employeeList = employeeService.getAll();
			for (Employee e : employeeList) {
				System.out.println(e);
			}

		} catch (SQLException e) {
			e.printStackTrace();
		}
	}

}
