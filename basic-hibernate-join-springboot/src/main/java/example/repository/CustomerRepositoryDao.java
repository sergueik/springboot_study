package example.repository;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.hibernate.Hibernate;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.transform.AliasToEntityMapResultTransformer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.data.domain.Example;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Component;

import example.model.Customer;
import example.model.CustomerItem;
import example.utils.HibernateUtility;

@Component
public class CustomerRepositoryDao implements CustomerRepository {

	private static Logger logger = LoggerFactory
			.getLogger(CustomerRepositoryDao.class);

	// NOTE: naming matters
	// otherwise:
	// java.lang.IllegalArgumentException: Failed to create query for method
	// public abstract void
	// example.repository.CustomerRepository.findCustomerDetailsById(int)! No
	// property id found for type Customer!

	@SuppressWarnings("deprecation")
	public List<CustomerItem> findCustomerDetailsByCustomerId(int customerId) {
		List<CustomerItem> data = new ArrayList<>();
		logger.info(
				"findCustomerDetailsByCustomerId processing customerId =" + customerId);
		SessionFactory factory = HibernateUtility.getSessionFactory();
		Session session = factory.openSession();
		// HQL
		@SuppressWarnings("unchecked")
		Query<Object[]> query = session
				.createQuery(
	      " select c.customerName, a.city, i.itemName,i.price, 'A' from Customer c join c.items i"
			+ " join c.addresses a  where c.customerId = :customerId"
			+ " union all "
			+ " select c.customerName, a.city, i.itemName, i.price, 'B' from Customer c join c.items i"
			+ " join c.addresses a  where c.customerId = :customerId")
				.setParameter("customerId", customerId);

		// NOTE:
		// With mySQL query with string function
		// if(a.city like 'atlanta', 'c', 's') city
		// or
		// regexp_replace(a.city, 'atlanta', 'a')
		// "trim()" works
		// works in plain JDBC
		// "select if (a.acity like 'atlanta', 'c', 's') as city from address a "
		// attempt to do the same through Hibernate with or without column alias
		// "as city"
		// leads to error in runtime:
		// java.lang.IllegalArgumentException:
		// org.hibernate.QueryException:
		// No data type for node: org.hibernate.hql.internal.ast.tree.MethodNode
		// with the ascii art presumable describing tne grammar lookahead parser
		// stop condition building AST
		// +-[METHOD_CALL] MethodNode: '('
		// | +-[METHOD_NAME] IdentNode: 'if' {originalText=if}
		// | \-[EXPR_LIST] SqlNode: 'exprList'
		// [select c.customerName, if (a.city like 'atlanta', 'c', 's') as city,
		// i.itemName,i.price from example.model.Customer c join c.items i join
		// c.addresses a where c.customerId = :customerId ]] with root cause
		//
		// org.hibernate.QueryException: No data type for node:
		// org.hibernate.hql.internal.ast.tree.MethodNode
		// +-[METHOD_CALL] MethodNode: '('
		// | +-[METHOD_NAME] IdentNode: 'if' {originalText=if}
		// | \-[EXPR_LIST] SqlNode: 'exprList'
		//
		// at
		// org.hibernate.hql.internal.ast.tree.SelectClause.initializeExplicitSelectClause(SelectClause.java:161)

		// NOTE:
		// " on c.customerId = a.cid " leads to
		// org.hibernate.hql.internal.ast.QuerySyntaxException: could not resolve
		// property: cid of: example.model.Address [select c.customerName, a.city,
		// i.itemName,i.price from example.model.Customer c join c.items i join
		// c.addresses a on c.customerId = a.cid where c.customerId = :customerId ]

		// NOTE: cannot dynamically extract ressult metadata column names
		// reuired to produce a targetClass instance
		// with specicic properties set through reflection
		// https://stackoverflow.com/questions/2605385/using-sql-column-names-in-hibernate-createsqlquery-result
		// Note: This is said to work for SQLQuery
		// Attent of using AliasToEntityMapResultTransformer on hql query without
		// specifying aliases
		// return index value as key
		try {
			Query query2 = session
					.createSQLQuery(
							"select c.customerName, a.city, i.itemName,i.price from Customer c "
									+ " join c.items i " + " join c.addresses a "
									+ " where c.customerId = :customerId ")
					.setParameter("customerId", customerId);

			query2.setResultTransformer(AliasToEntityMapResultTransformer.INSTANCE);
			List<Map<String, Object>> aliasToValueMapList = query2.list();
			logger.info("SQL Query key set: " + aliasToValueMapList.get(0).keySet());
		} catch (Exception e) {
			logger.info("exception (ignored): " + e.toString());
			// javax.persistence.PersistenceException:
			// could not extract ResultSet
			logger.info("exception cause: " + e.getCause().toString());
			// with root cause
			// org.hibernate.exception.SQLGrammarException:
			// could not extract ResultSet
			logger.info("exception cause(2): " + e.getCause().getCause().toString());
			// with root cause
			// SQL Error: 1142, SQLState: 42000
			// java.sql.SQLSyntaxErrorException:
			// SELECT command denied to user 'java'@'192.168.0.25' for table 'items'
			// in mysql logs see
			// Aborted connection 52 to db: 'test' user: 'java' host: '192.168.0.25'
			// (Got an error reading communication packets)
		}
		// TODO:
		// https://docs.jboss.org/hibernate/orm/4.2/javadocs/org/hibernate/type/Type.html
		List<Object[]> objectList = query.list();
		logger.info("Query result list: " + query.getResultList());
		Iterator<Object[]> objectIterator = objectList.iterator();
		while (objectIterator.hasNext()) {
			CustomerItem customerItem = new CustomerItem();
			Object rows[] = (Object[]) objectIterator.next();
			logger.info("Loading: " + rows[0] + "|" + rows[1] + "|" + rows[2] + "|"
					+ rows[3]);
			customerItem.setCustomerName(rows[0].toString());
			customerItem.setCustomerCity(rows[1].toString());
			customerItem.setItemName(rows[2].toString());
			customerItem.setPrice(Integer.parseInt(rows[3].toString()));
			data.add(customerItem);
		}
		session.clear();
		session.close();
		return (data);
	}

	// NOTE: this method is using left join for illustration.
	// it does not work - see the comment in the interface
	// commenting the findCustomerDetailsByCustomerId dows not help
	public List<CustomerItem> findAllCustomerItemsByCustomerId(int customerId) {
		List<CustomerItem> data = new ArrayList<>();
		logger.info("findAllCustomerDetails");
		SessionFactory factory = HibernateUtility.getSessionFactory();
		Session session = factory.openSession();
		// HQL
		// NOTE defining placeholder without filling with parameter will crash the
		// app completely
		// org.hibernate.QueryException: Named parameter not bound : customerId
		@SuppressWarnings("unchecked")
		Query<Object[]> query = session.createQuery(
				"select c.customerName, a.city, i.itemName,i.price from Customer c "
						+ "left join c.items i " + " join c.addresses a");
		// NOTE: cannot use "on c.customerId = a.cid" - leads to error in runtime:
		// antlr.SemanticException: could not resolve property: cid of:
		// example.model.Address
		List<Object[]> objectList = query.list();
		Iterator<Object[]> objectIterator = objectList.iterator();
		while (objectIterator.hasNext()) {
			CustomerItem customerItem = new CustomerItem();
			Object rows[] = (Object[]) objectIterator.next();
			logger.info("Loading: " + rows[0] + "|" + rows[1] + "|" + rows[2] + "|"
					+ rows[3]);
			customerItem.setCustomerName(rows[0].toString());
			customerItem.setCustomerCity(rows[1].toString());
			if (rows[2] != null)
				customerItem.setItemName(rows[2].toString());
			if (rows[3] != null)
				customerItem.setPrice(Integer.parseInt(rows[3].toString()));
			data.add(customerItem);
		}
		session.clear();
		session.close();
		return (data);
	}

	@Override
	public void deleteAllInBatch() {
	}

	@Override
	public void deleteInBatch(Iterable<Customer> arg0) {

	}

	@Override
	public List<Customer> findAll() {

		return null;
	}

	@Override
	public List<Customer> findAll(Sort arg0) {

		return null;
	}

	@Override
	public <S extends Customer> List<S> findAll(Example<S> arg0) {

		return null;
	}

	@Override
	public <S extends Customer> List<S> findAll(Example<S> arg0, Sort arg1) {

		return null;
	}

	@Override
	public List<Customer> findAllById(Iterable<Integer> arg0) {

		return null;
	}

	@Override
	public void flush() {

	}

	@Override
	public Customer getOne(Integer arg0) {

		return null;
	}

	@Override
	public <S extends Customer> List<S> saveAll(Iterable<S> arg0) {

		return null;
	}

	@Override
	public <S extends Customer> S saveAndFlush(S arg0) {

		return null;
	}

	@Override
	public Page<Customer> findAll(Pageable arg0) {

		return null;
	}

	@Override
	public long count() {

		return 0;
	}

	@Override
	public void delete(Customer arg0) {

	}

	@Override
	public void deleteAll() {

	}

	@Override
	public void deleteAll(Iterable<? extends Customer> arg0) {

	}

	@Override
	public void deleteById(Integer arg0) {

	}

	@Override
	public boolean existsById(Integer arg0) {

		return false;
	}

	@Override
	public Optional<Customer> findById(Integer arg0) {
		return null;
	}

	@Override
	public <S extends Customer> S save(S arg0) {
		return null;
	}

	@Override
	public <S extends Customer> long count(Example<S> arg0) {
		return 0;
	}

	@Override
	public <S extends Customer> boolean exists(Example<S> arg0) {
		return false;
	}

	@Override
	public <S extends Customer> Page<S> findAll(Example<S> arg0, Pageable arg1) {
		return null;
	}

	@Override
	public <S extends Customer> Optional<S> findOne(Example<S> arg0) {
		return null;
	}
}
