package example.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import example.model.Contact;

@Repository
public interface ContactRepository extends JpaRepository<Contact, Integer> {

}
