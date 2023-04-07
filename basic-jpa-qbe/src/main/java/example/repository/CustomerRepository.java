package example.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import example.model.Customers;

@Repository
public interface CustomerRepository extends JpaRepository<Customers, Long> {
	// NOTE: strongly typed
	@Query("SELECT new example.model.Customers(a.id, a.firstName,a.lastName,a.walletBalance)"
			+ " from Customers a where a.firstName LIKE '?1%' order by a.id")
	public List<Customers> findCustomers(
			@Param("firstNameFragment") String firstNameFragment);

	@Query(nativeQuery = true, value = "SELECT a.id, a.first_name, a.last_name, a.balance FROM customers a WHERE REGEXP_LIKE(a.first_name, ?1 ) order by a.first_name")
	public List<Object[]> queryCustomers(String firstName);

	public List<Customers> findAllByOrderByIdDesc();
}
