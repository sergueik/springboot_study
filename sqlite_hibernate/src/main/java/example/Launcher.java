package example;

import java.util.Properties;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

@SpringBootApplication
@ComponentScan(basePackages = { "example" })
@EnableJpaRepositories(basePackages = "example.repository")
// trouble when placing Controler and Repository into separate packages

public class Launcher {
	// public class SpringbootApplication /* #{class_name} */ {

	public static Properties prop = new Properties();

	public static void main(String[] args) throws Exception {

		// configuring the app through the application.yaml does not work

		/*
				InputStream input = null;
				try {
					input = SpringbootApplication.class.getClassLoader()
							.getResourceAsStream("application.properties");
					prop.load(input);
				} catch (IOException ex) {
					ex.printStackTrace();
				} finally {
					if (input != null) {
						try {
							input.close();
						} catch (IOException e) {
							e.printStackTrace();
						}
					}
				}
		
		
				Class.forName(prop.getProperty("spring.datasource.driver-class-name"));
		
				Connection connection = DriverManager.getConnection(
						"jdbc:#{jdbc_prefix}://#{database_host};databaseName=#{database_name}",
						"#{username}", "#{password}");
		
				Statement statement = null;
				String query = "select * from #{table}";
				try {
					statement = connection.createStatement();
					ResultSet resultSet = statement.executeQuery(query);
					while (resultSet.next()) {
						String id = resultSet.getString("ITEM_REFERENCE_ID");
						System.out.println("ITEM_REFERENCE_ID: " + id);
					}
				} catch (Exception e) {
					e.printStackTrace();
				} finally {
					if (statement != null) {
						statement.close();
					}
				}
		*/

		/*
		// occasionally getting
		org.sqlite.SQLiteException: [SQLITE_ERROR] SQL error or missing database (no such table: user)
		the below code creates table explicitly
		*/

		// DDL to create database table
		/* 
		System.err.println("Initilizing database");
		SessionFactory sessionFactory = new Configuration().configure().buildSessionFactory();
		Session session = sessionFactory.openSession();
		session.beginTransaction();
		String query = String.format(
				"CREATE TABLE `user` ( `id`	integer, `nick_name` varchar, `pass_word` varchar, `user_name` varchar, `user_gender` integer, PRIMARY KEY(`id`));");
		session.createSQLQuery(query);
		Transaction transaction = session.getTransaction();
		transaction.commit();
		session.close();
		System.err.println("Initilized database");
		*/
		SpringApplication.run(Launcher.class, args);
	}
}
