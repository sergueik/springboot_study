package bl;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

public class Util {

	private static final String DB_DRIVER = "org.h2.Driver";
	private static final String DB_URL = "jdbc:h2:~/test";
	private static final String DB_USERNAME = "ss";
	private static final String DB_PASSWORD = "";

	public Connection getConnection() {
		Connection connection = null;
		try {
			Class.forName(DB_DRIVER);
			connection = DriverManager.getConnection(DB_URL, DB_USERNAME, DB_PASSWORD);
			System.out.println("Connection OK");
		} catch (ClassNotFoundException | SQLException e) {
			e.printStackTrace();
			System.out.println("Connection ERROR");
		}
		return connection;
	}

}
