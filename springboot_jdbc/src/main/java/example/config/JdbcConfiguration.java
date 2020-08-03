package example.config;

import javax.sql.DataSource;

import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.jdbc.datasource.DriverManagerDataSource;

@Configuration
public class JdbcConfiguration {

	@Bean
	public DataSource dataSource() {
		/*
		 * ApplicationContext applicationContext = new ClassPathXmlApplicationContext(
		 * "classpath:applicationContext.xml");
		 * 
		 * // @Autowired DriverManagerDataSource dataSource = (DriverManagerDataSource)
		 * applicationContext .getBean("dataSource");
		 */
		DriverManagerDataSource dataSource = new DriverManagerDataSource();

		dataSource.setDriverClassName("com.mysql.jdbc.Driver");
		// should use mysql-server
		// dataSource.setUrl("jdbc:mysql://localhost:3306/cardb");
		dataSource.setUrl("jdbc:mysql://mysql-server:3306/cardb");
		dataSource.setUsername("cardbuser");
		dataSource.setPassword("123test321");
		return dataSource;
	}

	@Bean
	public NamedParameterJdbcTemplate namedParameterJdbcTemplate() {
		return new NamedParameterJdbcTemplate(dataSource());
	}

	@Bean
	public DataSourceTransactionManager txnManager() {
		return new DataSourceTransactionManager(dataSource());
	}
}
