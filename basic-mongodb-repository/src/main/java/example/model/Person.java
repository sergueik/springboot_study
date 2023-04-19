package example.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Document(collection = "Person")
public class Person {
	@Id
	private BigInteger id;
	private String name;
	private Optional<List<Ticket>> tickets;
	private List<Ticket> t = new ArrayList<>();

	public Optional<List<Ticket>> getTickets() {
		return (tickets == null || !tickets.isPresent())
				? Optional.of(new ArrayList<>()) : tickets;
	}

	public void setTickets(List<Ticket> data) {
		if (tickets == null || !tickets.isPresent()) {
			tickets = Optional.of(new ArrayList<>());
		}
		t = tickets.get();
		t.addAll(data);
		this.tickets = Optional.of(t);

	}

	public Person(@JsonProperty("id") BigInteger id,
			@JsonProperty("name") String name) {
		this.id = id;
		this.name = name;
	}

	public BigInteger getId() {
		return id;
	}

	public String getName() {
		return name;
	}

	public void setId(BigInteger id) {
		this.id = id;
	}

	public void setName(String name) {
		this.name = name;
	}

}
