package example.dao;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Repository;

import example.entity.Student;
import example.util.JDBCUtils;

@Repository("JdbcDao")
public class JDBCDao implements Dao {

	// http://www.baeldung.com/properties-with-spring
	// Autowired annotation is not supported on static fields
	@Value("${spring.datasource.url}")
	private String datasourceUrl;

	private static final Logger logger = Logger
			.getLogger(JDBCDao.class.getName());
	private static final Connection conn = JDBCUtils.getConnection();

	@Override
	public int addStudent(Student student) {
		int result = 0;
		String sql = "INSERT INTO student(name,course) VALUES (?,?)";
		try {
			PreparedStatement pre = conn.prepareStatement(sql);
			pre.setString(1, student.getName());
			pre.setString(2, student.getCourse());
			result = pre.executeUpdate();
		} catch (Exception ex) {
			logger.log(Level.SEVERE, null, ex);
		}
		return result;
	}

	@Override
	public List<?> findAllStudent() {
		logger.info("datasourceUrl = " + datasourceUrl);
		List<?> students = null;
		String sql = "SELECT * FROM student";
		try {
			PreparedStatement preparedStatement = conn.prepareStatement(sql);
			ResultSet resultSet = preparedStatement.executeQuery();
			students = JDBCUtils.TranverseToList(resultSet, Student.class);
		} catch (Exception ex) {
			logger.log(Level.SEVERE, null, ex);
		}
		return students;
	}

	@Override
	public int updateStudent(Student student) {
		int result = 0;
		String sql = "UPDATE student SET name = ?,course = ? WHERE id = ?";
		try {
			PreparedStatement pre = conn.prepareStatement(sql);
			pre.setString(1, student.getName());
			pre.setString(2, student.getCourse());
			pre.setLong(3, student.getId());
			result = pre.executeUpdate();
		} catch (Exception ex) {
			logger.log(Level.SEVERE, null, ex);
		}
		return result;
	}

	@Override
	public int delStudentById(long id) {
		int result = 0;
		String sql = "DELETE FROM student WHERE id = ?";
		try {
			PreparedStatement pre = conn.prepareStatement(sql);
			pre.setLong(1, id);
			result = pre.executeUpdate();
		} catch (Exception ex) {
			logger.log(Level.SEVERE, null, ex);
		}
		return result;
	}

	@Override
	public Student findStudentById(long id) {
		List<?> results = null;
		Student result = null;
		String sql = "SELECT * FROM student WHERE id = ?";
		try {
			PreparedStatement preparedStatement = conn.prepareStatement(sql);
			preparedStatement.setLong(1, id);
			ResultSet resultSet = preparedStatement.executeQuery();
			results = JDBCUtils.TranverseToList(resultSet, Student.class);
			if (results != null && results.size() != 0) {
				result = (Student) results.get(0);
			} else {

			}
		} catch (SQLException | InstantiationException | IllegalAccessException e) {
			logger.log(Level.SEVERE, null, e);
		}
		return result;
	}

	@Override
	public Student findStudentByName(String name) {
		List<?> results = null;
		Student result = null;
		String sql = "SELECT * FROM student WHERE name = ?";
		try {
			PreparedStatement preparedStatement = conn.prepareStatement(sql);
			preparedStatement.setString(1, name);
			ResultSet resultSet = preparedStatement.executeQuery();
			results = JDBCUtils.TranverseToList(resultSet, Student.class);
			if (results != null && results.size() != 0) {
				result = (Student) results.get(0);
			} else {

			}
		} catch (SQLException | InstantiationException | IllegalAccessException e) {
			logger.log(Level.SEVERE, null, e);
		}
		return result;
	}

	// mysql required connectionn string patch:
	// dataSource.setUrl("jdbc:mysql://localhost:3306/userdb" + "?useUnicode=true&useJDBCCompliantTimezoneShift=true&useLegacyDatetimeCode=false&serverTimezone=UTC");
	// https://github.com/Pragmatists/JUnitParams
	// http://www.cyberforum.ru/java-j2ee/thread2160223.html
        // for custom DAO implementing security tokens see
	// https://github.com/sebasv89/spring-boot-examples/tree/master/src/main/java/co/svelez/springbootexample/domain 
	// see also: ttps://stackoverflow.com/questions/36261216/how-to-rename-the-table-persistent-logins-in-spring-security
	// https://qna.habr.com/q/855545
}
