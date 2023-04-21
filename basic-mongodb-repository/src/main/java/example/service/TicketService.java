package example.service;

import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import example.model.Ticket;
import example.repository.TicketRepository;

@Service
public class TicketService {

	@Autowired
	private TicketRepository ticketRepository;

	public Optional<Ticket> getTicketById(String id) {
		return ticketRepository.findById(id);
	}

	public Ticket addNewApplication(Ticket ticket) {
		return ticketRepository.save(ticket);
	}

	public Ticket updateApplication(String id, Ticket ticket) {
		ticket.setId(id);
		return ticketRepository.save(ticket);
	}

	public void deleteTicket(String id) {
		ticketRepository.deleteById(id);
	}

}
