import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;
import java.sql.PreparedStatement;
import java.sql.CallableStatement;

public class MySQLJDBCMultiQueriesTest {
	public static void main(String[] argv) throws Exception {
		String className = "org.gjt.mm.mysql.Driver";
		try {
			Class driverObject = Class.forName(className);
			System.out.println("driverObject=" + driverObject);

			final String serverName = "localhost";
			final String databaseName = "information_schema";
			final String options = "allowMultiQueries=true&autoReconnect=true&useUnicode=true&characterEncoding=UTF-8";
			// Exception: Communications link failure
			final String url = "jdbc:mysql://" + serverName + "/" + databaseName + "?" + options;
			final String username = "java";
			final String password = "password";
			Connection connection = DriverManager.getConnection(url, username, "");

			if (connection != null) {
				System.out.println("Connected to product: " + connection.getMetaData().getDatabaseProductName());
				System.out.println("Connected to catalog: " + connection.getCatalog());

				// WARNING: combine statements. Exception: ResultSet is from UPDATE. No Data ?

				String query = "SELECT character_set_name from character_sets limit 1; SELECT character_set_name from character_sets";
				PreparedStatement preparedStatement = connection.prepareStatement(query);

				preparedStatement.execute();
				ResultSet resultSet = preparedStatement.getResultSet();
				while (resultSet.next()) {
					String name = resultSet.getString(1);
					// String description = resultSet.getString(2);
					System.out.println("character set: " + name);
					// System.out.println("description: " + description);
				}
				resultSet.close();
				preparedStatement.close();
				connection.close();
			} else {
				System.out.println("Failed to connect");
			}
		} catch (Exception e) {
			System.out.println("Exception: " + e.getMessage());
			e.printStackTrace();
		}
	}
}
