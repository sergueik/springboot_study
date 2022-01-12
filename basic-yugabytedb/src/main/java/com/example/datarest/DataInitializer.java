package com.example.datarest;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
@Transactional
public class DataInitializer {
	
	@Autowired
	PersonRepository repo;
	
	@PostConstruct
	public void init() {
		if (repo.count()>0) {
			return;
		}
		
		Person p1 = Person.of("John", "Smith");
		Person p2 = Person.of("Amy", "Brown");
		Person p3 = Person.of("Joe", "Doe");
		
		List<Person> persons = Stream.of(p1, p2, p3).collect(Collectors.toList());
		
		repo.saveAll(persons);
	}

}
