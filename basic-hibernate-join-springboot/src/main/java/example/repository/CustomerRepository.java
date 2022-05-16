package example.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import example.model.Customer;
import example.model.CustomerItem;
import example.model.Item;

public interface CustomerRepository extends JpaRepository<Customer, Integer> {

	public List<CustomerItem> findCustomerDetailsByCustomerId(int customerId);

	// NOTE: method naming is critical
	// try define the method named findAllCustomerDetails to get an
	// java.lang.IllegalArgumentException:
	// Failed to create query for method public abstract java.util.List
	// example.repository.CustomerRepository.findAllCustomerDetails()!
	// No property findAllCustomerDetails found for type Customer!
	// renaming with suffix ByCustomerId "fixes" the issue.
	// Leave it this way for now
	public List<CustomerItem> findAllCustomerItemsByCustomerId(int customerId);

	public List<CustomerItem> findCustomerDetailsViaNativeSQLByCustomerId(
			int customerId);
}
