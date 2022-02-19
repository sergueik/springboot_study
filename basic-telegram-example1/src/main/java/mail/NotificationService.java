package mail;

import Service.UserService;
import model.User;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@PropertySource("classpath:telegram.properties")
public class NotificationService {

	private final UserService userService;
	private final JavaMailSender emailSender;

	@Value("${bot.email.subject}")
	private String emailSubject;

	@Value("${bot.email.from}")
	private String emailFrom;

	@Value("${bot.email.to}")
	private String emailTo;

	public NotificationService(UserService userService,
			JavaMailSender emailSender) {
		this.userService = userService;
		this.emailSender = emailSender;
	}

	@Scheduled(fixedRate = 10000)
	public void sendNewApplications() {
		List<User> users = userService.findNewUser();
		if (users.size() == 0)
			return;

		StringBuilder sb = new StringBuilder();

		users.forEach(
				user -> sb.append("Телефон: ").append(user.getPhone()).append("\r\n")
						.append("Email: ").append(user.getEmail()).append("\r\n\r\n")

		);

		sendEmail(sb.toString());

	}

	private void sendEmail(String text) {

		SimpleMailMessage message = new SimpleMailMessage();
		message.setTo(emailTo);
		message.setFrom(emailFrom);
		message.setSubject(emailSubject);
		message.setText(text);
		emailSender.send(message);

	}
}
