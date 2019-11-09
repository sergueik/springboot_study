package org.utils.service;

import org.utils.entity.Result;
import org.utils.entity.Student;

public interface BaseService {

	public Result addStudent(Student student);
	public Result findAllStudent();
	public Result updateStudent(Student student);
	public Result delStudentById(String id);
	public Result findStudentById(String id);
	public Result findStudentByName(String name);
}
