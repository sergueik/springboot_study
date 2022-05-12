package example.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import example.model.Customer;
import example.model.Item;

public interface CustomerRepository extends JpaRepository<Customer, Integer> {

	
	public void findCustomerDetailsByCustomerId(int customerId);

}
