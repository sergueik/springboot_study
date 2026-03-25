package example.control.mail;

import example.control.mail.send.MailSendClient;
import example.control.persistence.NoteRepository;
import example.model.Mail;
import example.model.Note;

import org.springframework.stereotype.Service;

@Service
public final class NoteMailClient {
	private final NoteRepository noteRepository;
	private final MailSendClient sender;

	public NoteMailClient(NoteRepository noteRepository, MailSendClient sender) {
		this.noteRepository = noteRepository;
		this.sender = sender;
	}

	public void send(Note note) {
		sender.send(note.getEmail(), note.getTitle(), note.getText());
	}

	public void receive(Mail mail) {
		noteRepository.add(mail.getSubject(), mail.getContent(), mail.getFrom());
	}
}
