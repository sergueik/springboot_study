package com.bookportal.api.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.thymeleaf.context.Context;
import org.thymeleaf.spring5.SpringTemplateEngine;

@Service
@RequiredArgsConstructor
public class TemplateService {
    private final SpringTemplateEngine templateEngine;

    public String generateConfirmEmailTemplate(String url) {
        Context context = new Context();
        context.setVariable("url", url);
        return templateEngine.process("email-confirm.html",context);
    }
}
