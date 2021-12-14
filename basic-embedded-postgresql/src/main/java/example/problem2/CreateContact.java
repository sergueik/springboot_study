package example.problem2;

import java.sql.Timestamp;
import java.util.LinkedList;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import example.model.Contact;
import example.repository.ContactRepository;

@Component
public class CreateContact {

	@Autowired
	private ContactRepository repo;

	@PostConstruct
	public void init() {
		LinkedList<Contact> contacts = new LinkedList<>();
		for (int i = 10; i < 55; i++) {
			Contact contact = new Contact();
			contact.setId(i);
			contact.setAckmail("0");
			contact.setAddr1("Address 1 for : " + i);
			contact.setAddr2("Address 2 for : " + i);
			contact.setAddr3("Address 3 for : " + i);
			contact.setCivil("Civil : " + i);
			contact.setNom("Nom for : " + i);
			contact.setCodpos("COD : " + i);
			contact.setDialang("Dialang :  " + i);
			contact.setEmail(" test" + i + "@gmail.com");
			contact.setFax("123-233-33" + i);
			contact.setLastupdate(new Timestamp(System.currentTimeMillis()));
			contact.setLastupdateId(i + 20);
			contact.setNatact(" natact : " + i);
			contact.setPays("Pays : " + i);
			contact.setPrenom("Prenom : " + i);
			contact.setTel("234-333-44" + i);
			contact.setVille("Ville " + i);
			contacts.add(contact);
		}
		repo.saveAll(contacts);
	}
}
