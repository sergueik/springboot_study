package com.example.crawler;

import com.example.crawler.service.Crawl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ApplicationContext;

@SpringBootApplication
public class CrawlerApplication implements ApplicationRunner {

    private final String URL = "http://magento-test.finology.com.my/breathe-easy-tank.html";
    @Autowired
    private Crawl crawl;

    public static void main(String[] args) {
        ApplicationContext context = SpringApplication.run(CrawlerApplication.class, args);
    }

    @Override
    public void run(ApplicationArguments args) throws Exception {
        crawl.crawl(URL);
    }

}
