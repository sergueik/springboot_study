package example.service;

import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;

import example.dao.PersonDao;
import example.model.Person;
import example.model.Ticket;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

@Service
public class PersonService {

	private final PersonDao personDao;

	public PersonService(PersonDao personDao) {
		this.personDao = personDao;
	}

	public Person insertPersonData(Person person) {
		return personDao.insertPersonData(person);
	}

	public List<Person> findByNameLike(String nameRegex) {
		return personDao.findByNameLike(nameRegex);
	}

	public List<Person> findByNumberGreaterOrEqualCustomQuery(Integer number) {
		return personDao.findByNumberGreaterOrEqualCustomQuery(number);
	}

	public Collection<Person> getAllPersonInformation() {
		return personDao.getAllPersonInformation();
	}

	public Optional<Person> getPersonInformationUsingId(Integer id) {
		return personDao.getPersonInformationById(id);
	}

	public void updatePersonUsingId(Integer id, Person person) {
		personDao.updatePersonUsingId(id, person);
	}

	public void addTickets(Integer id, List<Ticket> tickets) {
		personDao.addTicketsPersonUsingId(id, tickets);
	}

	public void deletePersonUsingId(Integer id) {
		personDao.deletePersonUsingId(id);
	}

}
