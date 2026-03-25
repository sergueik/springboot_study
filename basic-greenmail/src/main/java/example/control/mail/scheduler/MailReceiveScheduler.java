package example.control.mail.scheduler;

import example.control.mail.NoteMailClient;
import example.control.mail.receive.MailReceiveClient;

import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
public class MailReceiveScheduler {

	private final MailReceiveClient mailReceiveClient;
	private final NoteMailClient noteMailClient;

	public MailReceiveScheduler(MailReceiveClient mailReceiveClient, NoteMailClient noteMailClient) {
		this.mailReceiveClient = mailReceiveClient;
		this.noteMailClient = noteMailClient;
	}

	@Scheduled(fixedRateString = "${mail.receive.schedule.interval.milliseconds}")
	public void receiveAndSaveNotes() {
		mailReceiveClient.receive().forEach(noteMailClient::receive);
	}
}
