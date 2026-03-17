package example.controller;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.containsString;

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(Controller.class)
public class ControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @Test
    public void cookieShouldContainEncodedValue42() throws Exception {

        MvcResult result = mockMvc.perform(get("/cookie"))
                .andExpect(status().isOk())
                .andReturn();

        String setCookie = result.getResponse().getHeader("Set-Cookie");

        // ensure cookie exists
        assertThat(setCookie, containsString("question="));

        // extract encoded value
        Pattern p = Pattern.compile("question=([^;]+)");
        Matcher m = p.matcher(setCookie);
        m.find();
        String encoded = m.group(1);

        // decode base64
        String decoded = new String(Base64.getDecoder().decode(encoded), StandardCharsets.UTF_8);

        // extract value
        Pattern p2 = Pattern.compile("value=(\\d+)");
        Matcher m2 = p2.matcher(decoded);
        m2.find();
        int value = Integer.parseInt(m2.group(1));

        // final assertion
        assertThat(value, is(42));
    }
}