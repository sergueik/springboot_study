package example.controller;

import java.util.Iterator;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import example.repository.AddressRepository;
import example.repository.StudentRepository;

import example.data.Student;
import example.data.Address;
import example.data.Gender;

import example.utils.HibernateUtility;

import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.SessionFactory;

@RestController
public class UserController {

	private Address address = null;

	private static final Logger LOGGER = LoggerFactory
			.getLogger(UserController.class);

	@Autowired
	private StudentRepository studentRepository;

	@Autowired
	private AddressRepository addressRepository;

	@GetMapping("/getUsers")
	public List<Student> getUsers() {
		LOGGER.info("processing \"/getUsers\"");
		return studentRepository.findAll();
	}

	@RequestMapping("/getUser")
	public Student getUser(Long id) {
		return studentRepository.findOne(id);
	}

	@PostMapping("/updateUser")
	public Student updateUser(@RequestBody Student targetUser) {
		Student user = studentRepository.findOne(targetUser.getId());
		if (targetUser.getUserName() != null) {
			user.setUserName(targetUser.getUserName());
		}
		if (targetUser.getPassword() != null) {
			user.setPassword(targetUser.getPassword());
		}

		if (targetUser.getGender() != null) {
			user.setGender(targetUser.getGender());
		}
		return studentRepository.saveAndFlush(user);
	}

	@DeleteMapping("/deleteUser")
	public void deleteUser(Long id) {
		Student user = studentRepository.findOne(id);
		studentRepository.delete(user);
	}

	// "Content-Type": "application/json"
	@PostMapping("/addUserObject")
	public Student addUserObject(@RequestBody Student newUser) {
		try {
			return studentRepository.saveAndFlush(newUser);
		} catch (Exception e) {
			return newUser;
		}
	}

	// x-form-data
	@RequestMapping(value = "/addUser", method = RequestMethod.POST)
	@ResponseBody
	public String addUser(@RequestParam("userName") String userName,
			@RequestParam("password") String password,
			@RequestParam("confirmPassword") String confirmPassword,
			@RequestParam("gender") Gender gender) {
		if (!(password.equals(confirmPassword))) {
			return "Password and confirmPassword do not match!";
		} else {
			List<Address> addresses = addressRepository.findAll();
			if (addresses.size() > 0)
				address = addresses.get(0);
			else
				address = new Address("street", "city", "state", "zip");
			studentRepository.save(new Student(userName, password, gender, address));
			return "Data added";
		}
	}

	// "Content-Type": "application/json"
	@PostMapping("/all")
	public void all(Long id) {
		try {
			SessionFactory factory = HibernateUtility.getSessionFactory();
			Session session = factory.openSession();
			// org.hibernate.hql.internal.ast.QuerySyntaxException: student
			// is not mapped
			// https://stackoverflow.com/questions/23018836/org-hibernate-hql-internal-ast-querysyntaxexception-table-is-not-mapped

			// org.hibernate.hql.internal.ast.QuerySyntaxException:
			// Path expected for join!
			Query qry = session.createQuery(
					"select s.userName, s.gender, a.city, a.state from User s "
							+ "left join Address a on a.id = s.address_id");
			List l = qry.list();
			Iterator it = l.iterator();
			while (it.hasNext()) {
				Object rows[] = (Object[]) it.next();
				LOGGER
						.info(rows[0] + " -- " + rows[1] + "--" + rows[2] + "--" + rows[3]);
			}
			session.clear();
			session.close();

		} catch (Exception e) {
			e.printStackTrace();

			LOGGER.error(e.toString());
		}
	}

}
