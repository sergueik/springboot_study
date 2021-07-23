package example;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Arrays;

import org.h2.tools.DeleteDbFiles;

public class ExampleApplication {

	private static final String DB_DRIVER = "org.h2.Driver";
	private static final String DB_CONNECTION = "jdbc:h2:~/test";
	private static final String DB_USER = "sa";
	private static final String DB_PASSWORD = "";

	public static void main(String[] args) throws Exception {
		try {
			DeleteDbFiles.execute("~", "test", true);
			createWithStatement();
			insertWithStatement();
			DeleteDbFiles.execute("~", "test", true);
			insertWithPreparedStatement();

		} catch (SQLException e) {
			e.printStackTrace();
		}
	}

	private static void insertWithPreparedStatement() throws SQLException {
		Connection connection = getDBConnection();
		PreparedStatement createPreparedStatement = null;
		PreparedStatement insertPreparedStatement = null;
		PreparedStatement selectPreparedStatement = null;

		String CreateQuery = "CREATE TABLE PERSON(id int primary key, name varchar(255))";
		String InsertQuery = "INSERT INTO PERSON" + "(id, name) values" + "(?,?)";
		String SelectQuery = "select * from PERSON";
		try {
			connection.setAutoCommit(false);

			createPreparedStatement = connection.prepareStatement(CreateQuery);
			createPreparedStatement.executeUpdate();
			createPreparedStatement.close();

			insertPreparedStatement = connection.prepareStatement(InsertQuery);
			insertPreparedStatement.setInt(1, 1);
			insertPreparedStatement.setString(2, "Jose");
			insertPreparedStatement.executeUpdate();
			insertPreparedStatement.close();

			selectPreparedStatement = connection.prepareStatement(SelectQuery);
			ResultSet rs = selectPreparedStatement.executeQuery();
			System.out.println("H2 Database inserted through PreparedStatement");
			while (rs.next()) {
				System.out
						.println("Id " + rs.getInt("id") + " Name " + rs.getString("name"));
			}
			selectPreparedStatement.close();

			connection.commit();
		} catch (SQLException e) {
			System.out.println("Exception Message " + e.getLocalizedMessage());
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			connection.close();
		}
	}

	private static void createWithStatement() throws SQLException {
		Connection connection = getDBConnection();
		Statement stmt = null;
		try {
			connection.setAutoCommit(false);
			for (String dml : Arrays.asList(
					"CREATE TABLE department ( department_id INT AUTO_INCREMENT PRIMARY KEY, department_name VARCHAR(100) NOT NULL); ",
					"CREATE TABLE employee ( employee_id INT AUTO_INCREMENT PRIMARY KEY, employee_firstname VARCHAR (100) NOT NULL, employee_lastname VARCHAR (100) NOT NULL, department_id INT NOT NULL, FOREIGN KEY (department_id) REFERENCES department (department_id) );")) {
				stmt = connection.createStatement();
				stmt.execute(dml);
				stmt.close();
			}
		} catch (SQLException e) {
			System.err.println("Exception: " + e.getLocalizedMessage());
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			connection.close();
		}

	}

	private static void insertWithStatement() throws SQLException {
		Connection connection = getDBConnection();
		Statement stmt = null;
		try {
			connection.setAutoCommit(false);
			stmt = connection.createStatement();
			stmt.execute(
					"CREATE TABLE PERSON(id int primary key, name varchar(255))");
			stmt.execute("INSERT INTO PERSON(id, name) VALUES(1, 'Anju')");
			stmt.execute("INSERT INTO PERSON(id, name) VALUES(2, 'Sonia')");
			stmt.execute("INSERT INTO PERSON(id, name) VALUES(3, 'Asha')");

			ResultSet rs = stmt.executeQuery("select * from PERSON");
			System.out.println("H2 Database inserted through Statement");
			while (rs.next()) {
				System.out
						.println("Id " + rs.getInt("id") + " Name " + rs.getString("name"));
			}
			stmt.close();
			connection.commit();
		} catch (SQLException e) {
			System.out.println("Exception Message " + e.getLocalizedMessage());
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			connection.close();
		}
	}

	private static Connection getDBConnection() {
		Connection dbConnection = null;
		try {
			Class.forName(DB_DRIVER);
		} catch (ClassNotFoundException e) {
			System.out.println(e.getMessage());
		}
		try {
			dbConnection = DriverManager.getConnection(DB_CONNECTION, DB_USER,
					DB_PASSWORD);
			return dbConnection;
		} catch (SQLException e) {
			System.out.println(e.getMessage());
		}
		return dbConnection;
	}
}
