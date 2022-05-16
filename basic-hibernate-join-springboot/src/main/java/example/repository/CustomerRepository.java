package example.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import example.model.Customer;
import example.projection.CustomerItem;

public interface CustomerRepository extends JpaRepository<Customer, Integer> {

	// @Query("select new example.projection.CustomerItem(c.customerName, a.city, i.itemName, i.price) from Customer c left join c.items i  join c.addresses a")
	// No property findAllCustomerItemsViaAnnotationQuery found for type Customer!
	public List<CustomerItem> findAllCustomerItemsViaAnnotationQueryByCustomerId(
			int customerId);
}
