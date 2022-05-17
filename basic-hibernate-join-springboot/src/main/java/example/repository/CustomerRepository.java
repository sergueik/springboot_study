package example.repository;

import java.util.Collection;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import example.model.Customer;
import example.projection.CustomerItem;

public interface CustomerRepository extends JpaRepository<Customer, Integer> {

	// TODO: add with parameter
	@Query("select c.customerName, a.city, i.itemName, i.price from Customer c left join c.items i join c.addresses a")
	public List<Object[]> findAllUntypedDataWithNulls();

	@Query("select c.customerName from Customer c")
	public List<String[]> findAllNames();

	@Query("SELECT c FROM Customer c")
	Collection<Customer> findAllCustomers();

	@Query("SELECT c FROM Customer c where c.customerId = ?1")
	Collection<Customer> findCustomer(int id);

	// broken projection example
	// @Query("SELECT new example.projection.CustomerItem(c.customerName,
	// a.city,i.itemName,i.price) from Customer c left outer join c.items i join
	// c.addresses a")

	@Query("SELECT new example.projection.CustomerItem(c.customerName, a.city,i.itemName,i.price)"
			+ " from Customer c join c.items i join c.addresses a")
	public List<CustomerItem> findAllCustomerItems();

}
