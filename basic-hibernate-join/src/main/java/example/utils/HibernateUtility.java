package example.utils;

import org.hibernate.SessionFactory;
import org.hibernate.cfg.Configuration;

public class HibernateUtility {
	private static SessionFactory factory;

	private HibernateUtility() {
	}

	@SuppressWarnings("deprecation")
	public synchronized static SessionFactory getSessionFactory() {
		if (factory == null) {
			factory = new Configuration().configure("hibernate.cfg.xml")
					.buildSessionFactory();
		}
		return factory;
	}

	@Override
	protected Object clone() throws CloneNotSupportedException {
		return new RuntimeException("Clone not Supported");
	}

}
