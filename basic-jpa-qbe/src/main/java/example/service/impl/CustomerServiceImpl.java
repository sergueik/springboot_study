package example.service.impl;

import static org.springframework.data.domain.ExampleMatcher.GenericPropertyMatchers.*;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.data.domain.ExampleMatcher.GenericPropertyMatchers;
import org.springframework.stereotype.Service;

import example.controller.CustomerController;
import example.model.Customers;
import example.repository.CustomerRepository;
import example.service.CustomerService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Service
public class CustomerServiceImpl implements CustomerService {

	private static Logger logger = LoggerFactory
			.getLogger(CustomerServiceImpl.class);

	@Autowired
	private CustomerRepository customerRepository;

	@Override
	public List<Customers> findByFirstNameStarting(String nameStarting) {
		return (List<Customers>) customerRepository.findCustomers(nameStarting);
	}

	@Override
	public List<Customers> findByFirstNameEnding(String ending) {

		Customers customers = Customers.builder().firstName(ending).build();
		ExampleMatcher matcher = ExampleMatcher.matching().withIgnoreNullValues()
				.withMatcher("firstName", match -> match.endsWith().ignoreCase(true));

		Example<Customers> example = Example.of(customers, matcher);
		return (List<Customers>) customerRepository.findAll(example);
	}

	@Override
	public List<Customers> findByLastNameEnding(String ending) {
		Customers customers = Customers.builder().lastName(ending).build();
		ExampleMatcher matcher = ExampleMatcher.matching().withMatcher("lastName",
				match -> match.endsWith().ignoreCase());

		Example<Customers> example = Example.of(customers, matcher);
		return (List<Customers>) customerRepository.findAll(example);
	}

	@Override
	public List<Customers> getAll() {
		return customerRepository.findAll();
	}

	@Override
	public List<Customers> findByFirstName(String firstName) {
		Customers customers = Customers.builder().firstName(firstName).build();
		ExampleMatcher matcher = ExampleMatcher.matching().withMatcher("firstName",
				exact().ignoreCase());

		Example<Customers> example = Example.of(customers, matcher);
		return (List<Customers>) customerRepository.findAll(example);
	}

	@Override
	public List<Customers> findByWalletBalanceEquals(Long balance) {
		Customers customers = Customers.builder().walletBalance(balance).build();
		ExampleMatcher matcher = ExampleMatcher.matching()
				.withMatcher("walletBalance", exact());

		Example<Customers> example = Example.of(customers, matcher);
		return (List<Customers>) customerRepository.findAll(example);

	}

	@Override
	public List<Object[]> queryCustomers(String firstName) {
		return customerRepository.queryCustomers(firstName);
	}
}
