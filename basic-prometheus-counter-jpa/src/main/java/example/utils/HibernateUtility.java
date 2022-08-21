package example.utils;

// NOTE:  methods buildSessionFactory and ServiceRegistryBuilder in Hibernate 4.3.4 are deprecated
// https://stackoverflow.com/questions/8640619/hibernate-serviceregistrybuilder
// import org.hibernate.service.ServiceRegistryBuilder;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.Configuration;
import org.hibernate.service.ServiceRegistry;
import org.hibernate.boot.registry.StandardServiceRegistryBuilder;

public class HibernateUtility {
	private static SessionFactory factory;

	protected HibernateUtility() {
	}

	public synchronized static SessionFactory getSessionFactory() {
		if (factory == null) {

			Configuration configuration = new Configuration();
			// NOTE: without specifying the confguration "hibernate.cfg.xml" will fail
			// in runtime with
			// Exception in thread "main" java.lang.NoSuchMethodError:
			// org.hibernate.Session.createQuery(Ljava/lang/String;)Lorg/hibernate/query/Query;
			// at example.Main.main(Main.java:19)

			// Without configuring with the error will be
			// WARN: HHH000342: Could not obtain connection to query metadata
			// java.lang.UnsupportedOperationException: The application must supply
			// JDBC connections
			// https://www.programcreek.com/java-api-examples/org.hibernate.cfg.Configuration
			// need full path ?
			// org.hibernate.internal.util.config.ConfigurationException: Could not
			// locate cfg.xml resource [hibernate.cfg.xml]
			String propertiesFileName = "hibernate.cfg.xml";
			String resourcePath = String.format("%s/src/main/resources/%s",
					System.getProperty("user.dir"), propertiesFileName);

			// configuration.configure(resourcePath);
			// org.hibernate.internal.util.config.ConfigurationException: 
			// Could not locate cfg.xml resource .../src/main/resources/hibernate.cfg.xml
			configuration.configure(propertiesFileName);

			factory = configuration.buildSessionFactory();

			// NOTE: the below initializaion will lead to error in runtime
			// Caused by: org.hibernate.hql.internal.ast.QuerySyntaxException:
			// Customer is not mapped
			// code kept for later study of the misconfiguration
			// ServiceRegistry serviceRegistry = new
			// StandardServiceRegistryBuilder().applySettings(configuration.getProperties()).build();
			// factory = configuration.buildSessionFactory(serviceRegistry);
		}
		return factory;
	}

	@Override
	protected Object clone() throws CloneNotSupportedException {
		return new RuntimeException("Clone not Supported");
	}

}
