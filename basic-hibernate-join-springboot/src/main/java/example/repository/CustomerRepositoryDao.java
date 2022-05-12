package example.repository;

import java.util.Iterator;
import java.util.List;
import java.util.Optional;

import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Component;

import example.model.Customer;
import example.utils.HibernateUtility;

@Component
public class CustomerRepositoryDao implements CustomerRepository {

	// NOTE: naming matters
	// otherwise:
	// java.lang.IllegalArgumentException: Failed to create query for method
	// public abstract void
	// example.repository.CustomerRepository.findCustomerDetailsById(int)! No
	// property id found for type Customer!

	public void findCustomerDetailsByCustomerId(int customerId) {
		SessionFactory factory = HibernateUtility.getSessionFactory();
		Session session = factory.openSession();
		// HQL
		Query query = session.createQuery(
				"select c.customerName, c.customerCity, i.itemName,i.price from Customer c "
						+ /* "left join c.items i" */ "join c.items i");
		List<Object[]> objectList = query.list();
		Iterator<Object[]> objectIterator = objectList.iterator();
		while (objectIterator.hasNext()) {
			Object rows[] = (Object[]) objectIterator.next();
			System.out.println(
					rows[0] + " -- " + rows[1] + "--" + rows[2] + "--" + rows[3]);
		}
		session.clear();
		System.err.println("cleared session");
		session.close();
		System.err.println("closed session");

	}

	@Override
	public void deleteAllInBatch() {
		// TODO Auto-generated method stub

	}

	@Override
	public void deleteInBatch(Iterable<Customer> arg0) {
		// TODO Auto-generated method stub

	}

	@Override
	public List<Customer> findAll() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<Customer> findAll(Sort arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <S extends Customer> List<S> findAll(Example<S> arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <S extends Customer> List<S> findAll(Example<S> arg0, Sort arg1) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<Customer> findAllById(Iterable<Integer> arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void flush() {
		// TODO Auto-generated method stub

	}

	@Override
	public Customer getOne(Integer arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <S extends Customer> List<S> saveAll(Iterable<S> arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <S extends Customer> S saveAndFlush(S arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Page<Customer> findAll(Pageable arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public long count() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public void delete(Customer arg0) {
		// TODO Auto-generated method stub

	}

	@Override
	public void deleteAll() {
		// TODO Auto-generated method stub

	}

	@Override
	public void deleteAll(Iterable<? extends Customer> arg0) {
		// TODO Auto-generated method stub

	}

	@Override
	public void deleteById(Integer arg0) {
		// TODO Auto-generated method stub

	}

	@Override
	public boolean existsById(Integer arg0) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public Optional<Customer> findById(Integer arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <S extends Customer> S save(S arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <S extends Customer> long count(Example<S> arg0) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public <S extends Customer> boolean exists(Example<S> arg0) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public <S extends Customer> Page<S> findAll(Example<S> arg0, Pageable arg1) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <S extends Customer> Optional<S> findOne(Example<S> arg0) {
		// TODO Auto-generated method stub
		return null;
	}
}
