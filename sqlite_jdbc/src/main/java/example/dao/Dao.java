package example.dao;

import java.util.List;

import example.entity.Result;
import example.entity.Student;

public interface Dao {

	public int addStudent(Student student);
	public List<?> findAllStudent();
	public int updateStudent(Student student);
	public int delStudentById(long id);
	// NOTE: strongly typed
	public Student findStudentById(long id);
	public Student findStudentByName(String name);	
}
