package example;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.web.server.ErrorPage;
import org.springframework.boot.web.server.WebServerFactoryCustomizer;
import org.springframework.boot.web.servlet.server.ConfigurableServletWebServerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.http.HttpStatus;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.web.servlet.config.annotation.ViewControllerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import example.dao.UserDetailsServiceDAO;

import javax.sql.DataSource;

import java.io.IOException;
import java.io.InputStream;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@SpringBootApplication
public class Launcher implements WebMvcConfigurer {

	@Autowired
	private DataSource dataSource;

	@Bean
	public JdbcTemplate jdbcTemplate() {
		return new JdbcTemplate(dataSource);
	}

	@Bean
	public UserDetailsService userDetailsService() {
		return new UserDetailsServiceDAO();
	}

	@Override
	public void addViewControllers(ViewControllerRegistry registry) {
		registry.addViewController("/").setViewName("home");
		registry.addViewController("/error").setViewName("error");
		registry.addViewController("/profile").setViewName("profile");
	}

	@Bean
	public WebServerFactoryCustomizer<ConfigurableServletWebServerFactory> webServerFactoryCustomizer() {
		return (factory) -> factory.addErrorPages(new ErrorPage(HttpStatus.NOT_FOUND, "/error"),
				new ErrorPage(HttpStatus.FORBIDDEN, "/error"));
	}

	public static void main(String[] args) {
		SpringApplication.run(Launcher.class, args);
	}

	public static void main_BROKEN(String[] args) {
		try {
			initDatabase();
			SpringApplication.run(Launcher.class, args);
		} catch (Exception e) {
			// TODO: explain
			// Exception (ignored):
			// org.springframework.boot.devtools.restart.SilentExitExceptionHandler$SilentExitException
			System.err.println("Exception (ignored): " + e.toString());
		}
	}

	private static void initDatabase() throws IOException, SQLException, ClassNotFoundException {
		System.err.println("Initilizing database");
		InputStream input = null;
		Connection connection;
		Statement statement = null;
		Properties properties = new Properties();
		input = Launcher.class.getClassLoader().getResourceAsStream("application.properties");
		properties.load(input);
		input.close();

		String query = String
				.format("CREATE TABLE IF NOT EXISTS `users` (\n" + "`username` VARCHAR(255) NOT NULL PRIMARY KEY, \n"
						+ "`password` VARCHAR(255) NOT NULL, \n" + "`role` VARCHAR(255) NOT NULL);\n" + "");

		Class.forName(properties.getProperty("spring.datasource.driver-class-name"));
		connection = DriverManager.getConnection(resolveEnvVars(properties.getProperty("spring.datasource.url")));
		// TODO: read resource
		query = String.format(
				"CREATE TABLE IF NOT EXISTS `users` (`username` VARCHAR(255) NOT NULL PRIMARY KEY, `password` VARCHAR(255) NOT NULL, `role` VARCHAR(255) NOT NULL, `reference_id` INT);");
		statement = connection.createStatement();
		statement.execute(query);
		query = "select * from users";
		statement = connection.createStatement();
		ResultSet resultSet = statement.executeQuery(query);
		while (resultSet.next()) {
			String id = resultSet.getString("REFERENCE_ID");
			System.out.println("REFERENCE_ID: " + id);
		}
		resultSet.close();
		statement.close();
		connection.close();
		System.err.println("Initilized database");
	}

	protected static String getScriptContent(String scriptName) {
		try {
			final InputStream stream = Launcher.class.getClassLoader().getResourceAsStream(scriptName);
			final byte[] bytes = new byte[stream.available()];
			stream.read(bytes);
			return new String(bytes, "UTF-8");
		} catch (IOException e) {
			throw new RuntimeException(scriptName);
		}
	}

	public static String resolveEnvVars(String input) {
		if (null == input) {
			return null;
		}
		Pattern p = Pattern.compile("\\$(?:\\{(?:env:)?(\\w+)\\}|(\\w+))");
		Matcher m = p.matcher(input);
		StringBuffer sb = new StringBuffer();
		while (m.find()) {
			String envVarName = null == m.group(1) ? m.group(2) : m.group(1);
			String envVarValue = System.getenv(envVarName);
			m.appendReplacement(sb, null == envVarValue ? "" : envVarValue.replace("\\", "\\\\"));
		}
		m.appendTail(sb);
		return sb.toString();
	}

}
