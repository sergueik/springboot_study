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

// Duplicate annotation of non-repeatable type @SuppressWarnings
// https://www.codejava.net/java-core/the-java-language/suppresswarnings-annotation-examples
@SuppressWarnings({ "unchecked", "deprecation" })

public class AdditionalQueriesImpl implements AdditionalQueries {
	private static Logger logger = LoggerFactory
			.getLogger(AdditionalQueries.class);
	private List<Object[]> result = new ArrayList<>();
	private Iterator<Object[]> nativeQueryObjectIterator;
	
	// NOTE: syntax 
	private Object rows[] = {};

	public List<Object[]> customFind(String query) {
		logger.info("customFind: {}", query);
		SessionFactory factory = HibernateUtility.getSessionFactory();
		Session session = factory.openSession();

		Query<Object[]> nativeQuery = session.createNativeQuery(query);
		// nativeQuery.setParameter("customerId", customerId);

		List<Object[]> nativeQueryObjectList = nativeQuery.list();
		result.clear();
		nativeQueryObjectIterator = nativeQueryObjectList.iterator();
		while (nativeQueryObjectIterator.hasNext()) {
			rows = (Object[]) nativeQueryObjectIterator.next();
			result.add(rows);
		}
		session.clear();
		session.close();
		return result;
	}

	public List<Object[]> customFind(String query, long id) {
		SessionFactory factory = HibernateUtility.getSessionFactory();
		Session session = factory.openSession();

		logger.info("customFind: {} {}", query, id);
		Query<Object[]> nativeQuery = session.createNativeQuery(query);
		nativeQuery.setParameter("customerId", id);

		List<Object[]> nativeQueryObjectList = nativeQuery.list();
		nativeQueryObjectIterator = nativeQueryObjectList.iterator();
		result.clear();
		while (nativeQueryObjectIterator.hasNext()) {
			rows = (Object[]) nativeQueryObjectIterator.next();
			result.add(rows);
		}
		session.clear();
		session.close();
		return result;
	}
}
