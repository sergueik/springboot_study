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

import example.entity.Host;
import example.utils.JDBCUtils;

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
	public int addHost(Host host) {
		int result = 0;
		String sql = "INSERT INTO hosts(hostname,app,environment,domain) VALUES (?,?,?,?)";
		try {
			PreparedStatement pre = conn.prepareStatement(sql);
			pre.setString(1, host.getHostname());
			pre.setString(2, host.getApp());
			pre.setString(3, host.getEnvironment());
			pre.setString(4, host.getDomain());
			result = pre.executeUpdate();
		} catch (Exception ex) {
			logger.log(Level.SEVERE, null, ex);
		}
		return result;
	}

	@Override
	public List<?> findAllHost() {
		logger.info("datasourceUrl = " + datasourceUrl);
		List<?> hosts = null;
		String sql = "SELECT * FROM hosts";
		try {
			PreparedStatement preparedStatement = conn.prepareStatement(sql);
			ResultSet resultSet = preparedStatement.executeQuery();
			hosts = JDBCUtils.TranverseToList(resultSet, Host.class);
		} catch (Exception ex) {
			logger.log(Level.SEVERE, null, ex);
		}
		return hosts;
	}

	@Override
	public int updateHost(Host host) {
		int result = 0;
		String sql = "UPDATE hosts SET hostname = ?,app = ?, environment = ?, domain = ? WHERE id = ?";
		try {
			PreparedStatement pre = conn.prepareStatement(sql);

			pre.setString(1, host.getHostname());
			pre.setString(2, host.getApp());
			pre.setString(3, host.getEnvironment());
			pre.setString(4, host.getDomain());
			pre.setLong(3, host.getId());
			result = pre.executeUpdate();
		} catch (Exception ex) {
			logger.log(Level.SEVERE, null, ex);
		}
		return result;
	}

	@Override
	public int delHostById(long id) {
		int result = 0;
		String sql = "DELETE FROM hosts WHERE id = ?";
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
	public Host findHostById(long id) {
		List<?> results = null;
		Host result = null;
		String sql = "SELECT * FROM hosts WHERE id = ?";
		try {
			PreparedStatement preparedStatement = conn.prepareStatement(sql);
			preparedStatement.setLong(1, id);
			ResultSet resultSet = preparedStatement.executeQuery();
			results = JDBCUtils.TranverseToList(resultSet, Host.class);
			if (results != null && results.size() != 0) {
				result = (Host) results.get(0);
			} else {

			}
		} catch (SQLException | InstantiationException | IllegalAccessException e) {
			logger.log(Level.SEVERE, null, e);
		}
		return result;
	}

	@Override
	public Host findHostByHostname(String hostname) {
		List<?> results = null;
		Host result = null;
		String sql = "SELECT * FROM hosts WHERE hostname = ?";
		try {
			PreparedStatement preparedStatement = conn.prepareStatement(sql);
			preparedStatement.setString(1, hostname);
			ResultSet resultSet = preparedStatement.executeQuery();
			results = JDBCUtils.TranverseToList(resultSet, Host.class);
			if (results != null && results.size() != 0) {
				result = (Host) results.get(0);
			} else {

			}
		} catch (SQLException | InstantiationException | IllegalAccessException e) {
			logger.log(Level.SEVERE, null, e);
		}
		return result;
	}

	// mysql required connectionn string patch:
	// dataSource.setUrl("jdbc:mysql://localhost:3306/userdb" +
	// "?useUnicode=true&useJDBCCompliantTimezoneShift=true&useLegacyDatetimeCode=false&serverTimezone=UTC");
	// https://github.com/Pragmatists/JUnitParams
	// http://www.cyberforum.ru/java-j2ee/thread2160223.html
	// for custom DAO implementing security tokens see
	// https://github.com/sebasv89/spring-boot-examples/tree/master/src/main/java/co/svelez/springbootexample/domain
	// see also:
	// ttps://stackoverflow.com/questions/36261216/how-to-rename-the-table-persistent-logins-in-spring-security
	// https://qna.habr.com/q/855545
}
