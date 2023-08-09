package com.pocisoft.eventsapi.data;

import org.hibernate.SessionFactory;
import org.hibernate.boot.registry.StandardServiceRegistry;
import org.hibernate.boot.registry.StandardServiceRegistryBuilder;
import org.hibernate.cfg.Configuration;

import com.pocisoft.eventsapi.AppSettings;

public class HibernateUtil {
    private static final SessionFactory sessionFactory;

    static {
        try {
            Configuration configuration = new Configuration();

            // Set the JDBC connection properties
            configuration.setProperty("hibernate.connection.driver_class", AppSettings.JDBC_DRIVER);

            configuration.setProperty("hibernate.connection.url",AppSettings.DB_URL);
            configuration.setProperty("hibernate.connection.username", AppSettings.USER);
            configuration.setProperty("hibernate.connection.password", AppSettings.PASS);

            configuration.setProperty("hibernate.dialect", "org.hibernate.dialect.MySQL8Dialect");
            configuration.setProperty("hibernate.show_sql", "true");
            configuration.addAnnotatedClass(Event.class);
            configuration.addAnnotatedClass(EventSubscription.class);
            configuration.addAnnotatedClass(UserAccount.class);
            StandardServiceRegistry serviceRegistry = new StandardServiceRegistryBuilder()
                    .applySettings(configuration.getProperties())
                    .build();

            sessionFactory = configuration.buildSessionFactory(serviceRegistry);
        } catch (Throwable ex) {
            throw new ExceptionInInitializerError(ex);
        }
    }

    public static SessionFactory getSessionFactory() {
        return sessionFactory;
    }
}
