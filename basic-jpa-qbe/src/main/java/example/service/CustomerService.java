package example.service;

import java.util.List;

import example.model.Customers;

public interface CustomerService {

	List<Customers> findByFirstNameEnding(String ending);
	List<Customers> findByLastNameEnding(String ending);
	List<Customers> findByFirstName(String firstName);
	List<Customers> getAll();
	List<Customers> findByWalletBalanceEquals(Long balance);
	List<Customers> findByFirstNameStarting(String nameStarting);
	List<Object[]> queryCustomers(String firstName);
	List<Customers> findAllCustomSorted();
	List<Customers> findAllAutoSorted();
}
