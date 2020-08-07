package example.config;

import javax.sql.DataSource;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.jdbc.datasource.DriverManagerDataSource;

@Configuration
public class JdbcConfiguration {

	private static final Logger logger = LogManager.getLogger(JdbcConfiguration.class);
	@SuppressWarnings("resource")
	@Bean
	public DataSource dataSource() {
		ApplicationContext applicationContext = new ClassPathXmlApplicationContext("classpath:applicationContext.xml");
		DriverManagerDataSource dataSource = (DriverManagerDataSource) applicationContext.getBean("dataSource");
		logger.info("Datasource URL: " + dataSource.getUrl());

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
