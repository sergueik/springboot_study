package example;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import example.Person;
import lombok.RequiredArgsConstructor;

@Component
@Transactional
public class DataInitializer {

	@Autowired
	PersonRepository repo;

	@PostConstruct
	public void init() {
		if (repo.count() > 0) {
			return;
		}
		// NOTE: eclipse Mars has trouble loading the @RequiredArgsConstructor
		// annotation
		// leading to compilation error in IDE
		// The method of(String, String) is undefined for the type Person
		//
		Person p1 = Person.of("John", "Smith");
		Person p2 = Person.of("Amy", "Brown");
		Person p3 = Person.of("Joe", "Doe");

		List<Person> persons = Stream.of(p1, p2, p3).collect(Collectors.toList());

		repo.saveAll(persons);
	}

}
