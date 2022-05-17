package example.repository;

import java.util.Collection;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import example.model.Customer;
import example.projection.CustomerItem;

public interface CustomerRepository extends JpaRepository<Customer, Integer> {

	// TODO: add with parameter
	@Query("select c.customerName, a.city, i.itemName, i.price from Customer c left join c.items i  join c.addresses a")
	public List<Object[]> findAllUntypedDataWithNulls(int customerId);

	@Query("select c.customerName from Customer c")
	public List<String[]> findAllNames(int customerId);

	// https://www.baeldung.com/spring-data-jpa-query
	@Query("SELECT c FROM Customer c")
	Collection<Customer> findAllCustomers();

	@Query("SELECT new example.projection.CustomerItem(c.customerName, a.city,i.itemName,i.price)"
			+ " from Customer c inner join c.items i join c.addresses a")
	public List<CustomerItem> findAllCustomerItems();

}
