package com.findinpath.unleash.web;

import com.findinpath.unleash.context.UnleashContextProvider;
import com.findinpath.unleash.model.qotd.Quote;
import no.finn.unleash.Unleash;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping("user")
public class UserController {

    private final Unleash unleash;
    private final UnleashContextProvider unleashContextProvider;

    public UserController(Unleash unleash,
                          UnleashContextProvider unleashContextProvider) {
        this.unleash = unleash;
        this.unleashContextProvider = unleashContextProvider;
    }

    @RequestMapping("/index")
    public String userIndex(Model model) {
        var qotdEnabled = unleash.isEnabled("qotd", unleashContextProvider.getUnleashContext());
        if (qotdEnabled) {
            var qotd = new Quote("Eckhart Tolle",
                    "The beginning of freedom is the realization that you are not the possessing entity the thinker.");
            model.addAttribute("qotd", qotd);
        }
        return "user/index";
    }


    @RequestMapping("/greeting")
    public String greeting(Model model) {

        var specialGreetingEnabled = unleash.isEnabled("specialGreeting", unleashContextProvider.getUnleashContext());
        if (specialGreetingEnabled) {
            model.addAttribute("specialGreeting", true);
        }
        return "user/greeting";
    }
}
