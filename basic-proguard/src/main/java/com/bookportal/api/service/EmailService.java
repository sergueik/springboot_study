package com.bookportal.api.service;

import com.bookportal.api.configs.EnvironmentVariables;
import com.bookportal.api.entity.PasswordReset;
import com.bookportal.api.entity.User;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.mail.javamail.MimeMessagePreparator;
import org.springframework.stereotype.Service;

import java.util.Date;

@Service
@RequiredArgsConstructor
public class EmailService {
    private final EmailConfirmService emailConfirmService;
    private final JavaMailSender emailSender;
    private final EnvironmentVariables env;
    private final TemplateService templateService;

    @Value("${spring.mail.username}")
    String mailAccountName;
    @Value("${server.port}")
    String port;
    @Value("${app.url}")
    String serverUrl;


    public void sendPasswordResetLink(PasswordReset passResetObj) {
        new Thread(() -> {
            String mail = passResetObj.getUser().getMail();
            MimeMessagePreparator mailMessage = mimeMessage -> {
                MimeMessageHelper message = new MimeMessageHelper(mimeMessage, true, "UTF-8");
                message.setSentDate(new Date());
                message.setFrom(mailAccountName, env.mailFrom());
                message.setTo(mail);
                message.setSubject(env.passwordResetMailSubject());
                message.setText(generatePassResetText(mail, passResetObj.getSecretKey()));
            };
            emailSender.send(mailMessage);
        }).start();
    }

    public void sendEmailConfirmationLink(User user) {
        emailConfirmService.generateEmailConfirmationKey(user)
                .doOnNext(emailConfirm -> {
                    String key = emailConfirm.getSecretKey();
                    MimeMessagePreparator mailMessage = mimeMessage -> {
                        MimeMessageHelper message = new MimeMessageHelper(mimeMessage, true, "UTF-8");
                        message.setSentDate(new Date());
                        message.setFrom(mailAccountName, env.mailFrom());
                        message.setTo(emailConfirm.getUser().getMail());
                        message.setSubject(env.verifyAccountMailSubject());
                        String confirmUrl = generateConfirmUrl(emailConfirm.getUser().getMail(), key);
                        message.setText(templateService.generateConfirmEmailTemplate(confirmUrl), true);
                    };
                    emailSender.send(mailMessage);
                }).subscribe();
    }

    private String generateConfirmUrl(String to, String key) {
        String url = serverUrl + ":" + port;
        return url + "/sendMail/confirmEmail?key=" + key + "&email=" + to;
    }

    private String generatePassResetText(String to, String key) {
        return "Mail adresi: " + to + "  Key:" + key;
    }
}