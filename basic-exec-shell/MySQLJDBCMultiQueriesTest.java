import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;
import java.sql.PreparedStatement;
import java.sql.CallableStatement;

public class MySQLJDBCMultiQueriesTest {
	public static void main(String[] argv) throws Exception {
		String className = "com.mysql.cj.jdbc.Driver";
		try {
			Class driverObject = Class.forName(className);
			System.out.println("driverObject=" + driverObject);

			final String serverName = "172.17.0.2";
			final String databaseName = "information_schema";
			final String options = "allowMultiQueries=true&autoReconnect=true&useUnicode=true&characterEncoding=UTF-8";
			// Exception: Communications link failure
			final String url = "jdbc:mysql://" + serverName + "/" + databaseName + "?" + options;
			System.out.println("Connecting with: " + url);
			final String username = "java";
			final String password = "password";
			Connection connection = DriverManager.getConnection(url, username, password);

			if (connection != null) {
				System.out.println("Connected to product: " + connection.getMetaData().getDatabaseProductName());
				System.out.println("Connected to catalog: " + connection.getCatalog());

				// WARNING: when combine SQL statements only the first one gets executed
				// the second and following are ignored
				// below, even the field list is different
				String query = "SELECT character_set_name, description from character_sets where character_set_name like 'utf%' limit 1;"
						+ "SELECT character_set_name from character_sets where description not like '%unicode%';" 
            + "SELECT character_set_name from character_sets where character_set_name like 'utf8' limit 1;";
				System.out.println("Executing combined query: " + query);

				PreparedStatement preparedStatement = connection.prepareStatement(query);

				preparedStatement.execute();
				ResultSet resultSet = preparedStatement.getResultSet();
				while (resultSet.next()) {
					String name = resultSet.getString(1);
					String description = resultSet.getString(2);
					System.out.println("character set: " + name);
					System.out.println("description: " + description);
				}
				resultSet.close();
				preparedStatement.close();
				connection.close();
			} else {
				System.out.println("Failed to connect");
			}
		} catch (Exception e) {
			// java.sql.SQLNonTransientConnectionException:
			System.out.println("Exception: " + e.getMessage());
			e.printStackTrace();
		}
	}
}
