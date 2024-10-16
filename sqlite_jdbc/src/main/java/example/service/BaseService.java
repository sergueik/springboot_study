package example.service;

import example.entity.Result;
import example.entity.Student;

public interface BaseService {

	public Result addStudent(Student student);
	public Result findAllStudent();
	public Result updateStudent(Student student);
	public Result delStudentById(String id);
	public Result findStudentById(String id);
	public Result findStudentByIdWithCTE(String id);
	public Result findStudentByName(String name);
	
	// new method for joins
	public Result all(String id);
}
