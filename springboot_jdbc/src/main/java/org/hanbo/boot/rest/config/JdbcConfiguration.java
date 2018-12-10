package org.hanbo.boot.rest.config;

import javax.sql.DataSource;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.jdbc.datasource.DriverManagerDataSource;

@Configuration
public class JdbcConfiguration
{
   @Bean
   public DataSource dataSource()
   {
      DriverManagerDataSource dataSource = new DriverManagerDataSource();

      dataSource.setDriverClassName("com.mysql.jdbc.Driver");
      dataSource.setUrl("jdbc:mysql://localhost:3306/cardb");
      dataSource.setUsername("cardbuser");
      dataSource.setPassword("123test321");

      return dataSource;
   }
   
   @Bean
   public NamedParameterJdbcTemplate namedParameterJdbcTemplate()
   {
      NamedParameterJdbcTemplate retBean
          = new NamedParameterJdbcTemplate(dataSource());
       return retBean;
   }

   @Bean
   public DataSourceTransactionManager txnManager()
   {
      DataSourceTransactionManager txnManager
         = new DataSourceTransactionManager(dataSource());
      return txnManager;
   }
}
