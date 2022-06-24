package example.repository;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import java.util.List;
import org.hibernate.Hibernate;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.transform.AliasToEntityMapResultTransformer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import example.model.Server;
import example.model.Instance;
import example.model.Application;
import example.utils.HibernateUtility;

public class AdditionalQueriesImpl implements AdditionalQueries {
	private static Logger logger = LoggerFactory
			.getLogger(AdditionalQueries.class);

	public List<Object[]> customFind(String query) {
		List<Object[]> result = new ArrayList<>();
		logger.info("customFind");
		SessionFactory factory = HibernateUtility.getSessionFactory();
		Session session = factory.openSession();

		@SuppressWarnings("unchecked")
		Query<Object[]> nativeQuery = session.createNativeQuery(query);
		// nativeQuery.setParameter("customerId", customerId);
		List<Object[]> nativeQueryObjectList = nativeQuery.list();
		Iterator<Object[]> nativeQueryObjectIterator = nativeQueryObjectList
				.iterator();
		while (nativeQueryObjectIterator.hasNext()) {
			Object rows[] = (Object[]) nativeQueryObjectIterator.next();
			result.add(rows);
		}
		session.clear();
		session.close();
		return result;
	}
}
