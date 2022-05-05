package example.service;

import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.annotation.Resource;
import org.springframework.stereotype.Service;

import example.dao.Dao;
import example.dao.JDBCDao;
import example.entity.Result;
import example.entity.Student;

@Service
public class ServiceImpl implements BaseService {

	@Resource(name = "JdbcDao")
	private Dao dao;
	
	private static final Logger logger = Logger
			.getLogger(ServiceImpl.class.getName());

	@Override
	public Result addStudent(Student student) {
		Result result = new Result();
		try {
			int res = dao.addStudent(student);
			result.setStatus(res);
		} catch (Exception e) {
			logger.log(Level.SEVERE, null, e);
		}
		return result;
	}

	@Override
	public Result findAllStudent() {
		Result result = new Result();
		try {
			List<?> students = dao.findAllStudent();
			result.setStatus(1);
			result.setData(students);
		} catch (Exception e) {
			logger.log(Level.SEVERE, null, e);
		}
		return result;
	}

	@Override
	public Result updateStudent(Student student) {
		Result result = new Result();
		try {
			int res = dao.updateStudent(student);
			result.setStatus(res);
		} catch (Exception e) {
			logger.log(Level.SEVERE, null, e);
		}
		return result;
	}

	@Override
	public Result delStudentById(String id) {
		Result result = new Result();
		try {
			int res = dao.delStudentById(Long.parseLong(id));
			result.setStatus(res);
		} catch (Exception e) {
			logger.log(Level.SEVERE, null, e);
		}
		return result;
	}

	@Override
	public Result findStudentById(String id) {
		Result result = new Result();
		try {
			Student res = dao.findStudentById(Long.parseLong(id));
			result.setData(res);
		} catch (Exception e) {
			logger.log(Level.SEVERE, null, e);
		}
		return result;
	}

	@Override
	public Result findStudentByName(String name) {
		Result result = new Result();
		try {
			Student res = dao.findStudentByName(name);
			result.setData(res);
		} catch (Exception e) {
			logger.log(Level.SEVERE, null, e);
		}
		return result;
	}

	// @Override
	public Result all(String id) {
		Result result = new Result();
		try {
			List<?> studentsaddresses = ((JDBCDao) dao).all(Long.parseLong(id));
			result.setStatus(1);
			result.setData(studentsaddresses);
		} catch (Exception e) {
			// TODO: switch logger
			logger.log(Level.SEVERE, null, e);
		}
		return result;
	}
}
