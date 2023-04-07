package example.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import example.model.Customers;

@Repository
public interface CustomerRepository extends JpaRepository<Customers, Long> {
	// NOTE: strongly typed
	@Query("SELECT new example.model.Customers(a.id, a.firstName,a.lastName,a.walletBalance)"
			+ " from Customers a where a.firstName LIKE ?1%")
	public List<Customers> findCustomers(String firstNameFragment);
}
