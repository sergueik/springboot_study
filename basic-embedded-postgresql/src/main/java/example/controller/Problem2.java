package example.controller;

import java.util.List;

import javax.persistence.EntityManager;

import org.hibernate.Session;
import org.hibernate.query.Query;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import example.problem2.Contact;
import example.repository.ContactRepository;

/**
 * Using this class create a rest API which will return data from an embedded postgres
 */
@RestController
@RequestMapping("/api/db/v1")
public class Problem2 {

	private EntityManager entityManager;

	@Autowired
	private ContactRepository repository;

	@GetMapping(value = "/data")
	public ResponseEntity<List<example.model.Contact>> getContacts() {
		// System.out.println(repository.findAll());
		return ResponseEntity.ok().body(repository.findAll());
	}

	public List<Contact> findAll() {

		// get the current hibernate session
		Session session = entityManager.unwrap(Session.class);

		// create a query
		Query<Contact> query = session.createQuery("from contact", Contact.class);

		// execute query and get result list
		List<Contact> list = query.getResultList();

		// return the result

		return list;
	}
}
