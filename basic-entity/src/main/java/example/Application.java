package example;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ApplicationContext;

import example.service.Crawl;

@SpringBootApplication
public class Application implements ApplicationRunner {

    private final String URL = "http://magento-test.finology.com.my/breathe-easy-tank.html";
    @Autowired
    private Crawl crawl;

    public static void main(String[] args) {
        ApplicationContext context = SpringApplication.run(Application.class, args);
    }

    @Override
    public void run(ApplicationArguments args) throws Exception {
        crawl.crawl(URL);
    }

}
