package com.findinpath.unleash.web;

import no.finn.unleash.Unleash;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
public class MainController {

    private final Unleash unleash;

    public MainController(Unleash unleash){
        this.unleash = unleash;
    }

    @RequestMapping("/")
    public String root() {
        return "redirect:/index";
    }

    @RequestMapping("/index")
    public String index(Model model) {
        return "index";
    }
}
