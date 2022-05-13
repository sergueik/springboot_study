package example.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import example.model.Customer;
import example.model.CustomerItem;
import example.model.Item;

public interface CustomerRepository extends JpaRepository<Customer, Integer> {

	public List<CustomerItem> findCustomerDetailsByCustomerId(int customerId);

	/// java.lang.IllegalArgumentException:
	// Failed to create query for method public abstract java.util.List
	/// example.repository.CustomerRepository.findAllCustomerDetails()! No
	/// property findAllCustomerDetails found for type Customer!
	// public List<CustomerItem> findAllCustomerDetails();
}
