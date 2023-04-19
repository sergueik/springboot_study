package example.dao;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import example.model.Person;
import example.model.Ticket;
import example.repository.PersonRepository;
import example.repository.TicketRepository;

import java.util.Collection;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.math.BigInteger;
import java.nio.ByteBuffer;

@Component
public class PersonDao {

	private final PersonRepository personRepository;
	@Autowired
	private TicketRepository ticketRepository;

	public PersonDao(PersonRepository personRepository) {
		this.personRepository = personRepository;
	}

	public Person insertPersonData(Person person) {
		return personRepository.insert(person);
	}

	public Collection<Person> getAllPersonInformation() {
		return personRepository.findAll();
	}

	public Optional<Person> getPersonInformationById(Integer id) {
		return personRepository.findById(convertInt(id));
	}

	public List<Person> findByNameLike(String nameRegex) {
		return personRepository.findByNameLike(nameRegex);
	}

	public List<Person> findByNumberGreaterOrEqualCustomQuery(Integer number) {
		return personRepository
				.findByNumberGreaterOrEqualCustomQuery(convertInt(number));
	}

	public Person updatePersonUsingId(Integer id, Person person) {
		Optional<Person> findPersonQuery = personRepository
				.findById(convertInt(id));
		Person personValues = findPersonQuery.get();
		personValues.setId(person.getId());
		personValues.setName(person.getName());
		return personRepository.save(personValues);
	}

	public Person addTicketsPersonUsingId(Integer id, List<Ticket> tickets) {
		Optional<Person> findPersonQuery = personRepository
				.findById(convertInt(id));
		Person personValues = findPersonQuery.get();
		personValues.setTickets(tickets);
		return personRepository.save(personValues);
	}

	public void deletePersonUsingId(Integer id) {
		try {
			personRepository.deleteById(convertInt(id));
		} catch (NoSuchElementException e) {
			e.printStackTrace();
		}
	}

	// http://www.java2s.com/example/java/java.math/convert-int-to-biginteger.html
	public static int NUMBER_OF_BYTES_INT = Integer.SIZE / Byte.SIZE;

	public static BigInteger convertInt(int value) {
		return new BigInteger(
				ByteBuffer.allocate(NUMBER_OF_BYTES_INT).putInt(value).array());
	}
}
