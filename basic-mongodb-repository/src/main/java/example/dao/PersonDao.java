package example.dao;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import example.model.Person;
import example.repository.PersonRepository;

import java.util.Collection;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Optional;

@Component
public class PersonDao {

	private final PersonRepository personRepository;

	public PersonDao(PersonRepository personRepository) {
		this.personRepository = personRepository;
	}

	public Person insertPersonData(Person person) {
		return personRepository.insert(person);
	}

	public Collection<Person> getAllPersonInformation() {
		return personRepository.findAll();
	}

	public Optional<Person> getPersonInformationById(String id) {
		return personRepository.findById(id);
	}

	public List<Person> findByNameLike(String nameRegex) {
		return personRepository.findByNameLike(nameRegex);
	}

	public List<Person> findByNumberGreaterOrEqualCustomQuery(int number) {
		return personRepository.findByNumberGreaterOrEqualCustomQuery(number);
	}

	public Person updatePersonUsingId(String id, Person person) {
		Optional<Person> findPersonQuery = personRepository.findById(id);
		Person personValues = findPersonQuery.get();
		personValues.setId(person.getId());
		personValues.setName(person.getName());
		return personRepository.save(personValues);
	}

	public void deletePersonUsingId(String id) {
		try {
			personRepository.deleteById(id);
		} catch (NoSuchElementException e) {
			e.printStackTrace();
		}
	}

}
