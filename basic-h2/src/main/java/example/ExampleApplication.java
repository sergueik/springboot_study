package example;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

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
			// DeleteDbFiles.execute("~", "test", true);
			insertWithPreparedStatement();
			queryWithPreparedStatement();

			System.out.println("Press Enter to quit");
			// https://www.baeldung.com/java-console-input-output
			// this expects a string - plain Enter is not accepted
			/*
			Scanner scanner = new Scanner(System.in);
			String dummy = scanner.next();
			*/

			BufferedReader buffReader = new BufferedReader(
					new InputStreamReader(System.in));

			buffReader.readLine();

		} catch (SQLException e) {
			e.printStackTrace();
		}
	}

	private static void queryWithPreparedStatement() throws SQLException {
		Connection connection = getDBConnection();
		PreparedStatement statement = null;

		String query = "SELECT employee_id, employee_firstname, employee_lastname, department_name FROM employee e "
				+ "JOIN department d ON e.department_id = d.department_id "
				+ "WHERE employee_firstname = ? AND " + "employee_lastname = ?";
		List<String> employee = Arrays.asList("Bob", "Smith");
		try {
			connection.setAutoCommit(false);
			statement = connection.prepareStatement(query);

			statement.setString(1, employee.get(0));
			statement.setString(2, employee.get(1));

			ResultSet rs = statement.executeQuery();
			System.out.println("H2 Database queried through PreparedStatement");
			while (rs.next()) {
				System.out.println("Id: " + rs.getInt("employee_id") + " First Name: "
						+ rs.getString("employee_firstname") + " Last Name: "
						+ rs.getString("employee_lastname") + " Department name: "
						+ rs.getString("department_name"));
			}
			statement.close();
			connection.commit();
		} catch (SQLException e) {
			System.out.println("Exception Message " + e.getLocalizedMessage());
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			connection.close();
		}
	}

	private static void insertWithPreparedStatement() throws SQLException {
		Connection connection = getDBConnection();
		PreparedStatement insertPreparedStatement = null;
		PreparedStatement selectPreparedStatement = null;

		String InsertQuery = "INSERT INTO employee (employee_id, employee_firstname, employee_lastname, department_id) VALUES (?,?,?,?)";
		String SelectQuery = "select * from employee";
		List<ArrayList<Object>> data = new ArrayList<>(Arrays.asList(
				new ArrayList<Object>(Arrays.asList(1, "Bob", "Smith", 4)),
				new ArrayList<Object>(Arrays.asList(2, "John", "Green", 1)),
				new ArrayList<Object>(Arrays.asList(3, "Sally", "Wilson", 3)),
				new ArrayList<Object>(Arrays.asList(4, "Harold", "Smith", 2)),
				new ArrayList<Object>(Arrays.asList(5, "Joe", "White", 5)),
				new ArrayList<Object>(Arrays.asList(6, "Arnold", "Shoemaker", 4))));
		try {
			connection.setAutoCommit(false);
			insertPreparedStatement = connection.prepareStatement(InsertQuery);

			for (List<Object> row : data) {
				insertPreparedStatement.setInt(1, (int) row.get(0));
				insertPreparedStatement.setString(2, row.get(1).toString());
				insertPreparedStatement.setString(3, row.get(2).toString());
				insertPreparedStatement.setInt(4, (int) row.get(3));
				insertPreparedStatement.executeUpdate();
			}
			insertPreparedStatement.close();

			selectPreparedStatement = connection.prepareStatement(SelectQuery);
			ResultSet rs = selectPreparedStatement.executeQuery();
			System.out.println("H2 Database inserted through PreparedStatement");
			while (rs.next()) {
				System.out.println("Id: " + rs.getInt("employee_id") + " First Name: "
						+ rs.getString("employee_firstname") + " Last Name: "
						+ rs.getString("employee_lastname") + " Department Id: "
						+ rs.getInt("department_id"));
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
			stmt = connection.createStatement();
			for (String dml : Arrays.asList(
					"CREATE TABLE department ( department_id INT AUTO_INCREMENT PRIMARY KEY, department_name VARCHAR(100) NOT NULL); ",
					"CREATE TABLE employee ( employee_id INT AUTO_INCREMENT PRIMARY KEY, employee_firstname VARCHAR (100) NOT NULL, employee_lastname VARCHAR (100) NOT NULL, department_id INT NOT NULL, FOREIGN KEY (department_id) REFERENCES department (department_id) );")) {
				stmt.execute(dml);
			}
			stmt.close();
		} catch (SQLException e) {
			System.err.println("Exception: " + e.getLocalizedMessage());
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			connection.close();
		}

	}

	private static void insertWithStatement() throws SQLException {

		String[] departments = new String[] { "Sales", "Marketing",
				"Human Resources", "Manufacturing", "Accounting" };

		Connection connection = getDBConnection();
		Statement stmt = null;
		try {
			connection.setAutoCommit(false);
			stmt = connection.createStatement();
			for (int cnt = 0; cnt != departments.length; cnt++) {
				// numbering from 1
				stmt.execute(String.format(
						"INSERT INTO department (department_id, department_name) VALUES (%d, '%s')",
						cnt + 1, departments[cnt]));
			}

			ResultSet rs = stmt.executeQuery("select * from department");
			System.out.println("H2 Database inserted through Statement");
			while (rs.next()) {
				System.out.println("Id " + rs.getInt("department_id") + " Name "
						+ rs.getString("department_name"));
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
