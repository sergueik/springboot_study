package org.utils.springboot;

import org.hibernate.Hibernate;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.hibernate.cfg.Configuration;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;

@SpringBootApplication
@ComponentScan(basePackages = { "org.utils" })
public class SpringbootApplication {

	public static void main(String[] args) {

		/*
		// occasionally getting
		org.sqlite.SQLiteException: [SQLITE_ERROR] SQL error or missing database (no such table: user)
		the below code creates table explicitly
		*/
		/* 
				SessionFactory sessionFactory = new Configuration().configure()
						.buildSessionFactory();
		
		    Session session = sessionFactory.openSession();
				session.beginTransaction();
				String query = String.format(
						"CREATE TABLE `user` ( `id`	integer, `nick_name` varchar, `pass_word` varchar, `user_name` varchar, `user_gender` integer, PRIMARY KEY(`id`));");
				session.createSQLQuery(query);
				Transaction transaction = session.getTransaction();
				transaction.commit();
				session.close();
		*/
		SpringApplication.run(SpringbootApplication.class, args);
	}

}
