package com.example.crawler.service;

import com.example.crawler.model.Product;
import com.example.crawler.repository.ProductRepository;
import com.example.crawler.util.Translator;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.example.crawler.util.Constants.*;

@Service
public class Crawl {
    @Autowired
    ProductRepository productRepository;
    private Queue<String> urlQueue = new LinkedList<>();
    private Set<String> visitedList = new HashSet<>();

    public void crawl(String url) {
        urlQueue.add(url);
        System.out.println("Waiting queue size: " + urlQueue.size());
        Document document;

        while (!urlQueue.isEmpty()) {
            String currentUrl = urlQueue.poll();
            if (visitedList.contains(currentUrl)) {
                checkWeatherCrawlingIsFinished();
                continue;
            }
            try {
                System.out.println("Fetching data from " + currentUrl);
                document = Jsoup.connect(currentUrl).get();

                findAllLinksInCurrentUrlAndAddToQueue(document);

                String content = document.select("meta[property=og:type]").attr("content");
                if (!content.equals("product")) {
                    visitedList.add(currentUrl);
                    System.out.println("Url is added to ignore list.");
                    System.out.println("Ignore list size: " + visitedList.size());
                    System.out.println("Waiting queue size: " + urlQueue.size());
                    checkWeatherCrawlingIsFinished();
                    continue;
                }
                Map<String, String> productInfoMap = getProductInfo(document);
                Product product = Translator.personTranslator(productInfoMap);

                productRepository.save(product);
                System.out.println("One product is added to database.");

                visitedList.add(currentUrl);

            } catch (IOException e) {
                e.printStackTrace();
            }
            System.out.println("Waiting queue size: " + urlQueue.size());
            checkWeatherCrawlingIsFinished();
        }
    }

    /**
     * Find all links in document and adds new links to the queue.
     *
     * @param document
     */
    private void findAllLinksInCurrentUrlAndAddToQueue(Document document) {
        Elements links = document.select("a[href]");
        System.out.println(links.size() + " links found.");
        int i = 0;
        for (Element link : links) {
            String linkStr = link.attr("abs:href");
            if (linkStr.contains("#")) {
                linkStr = linkStr.substring(0, linkStr.indexOf("#"));
            }
            if (linkStr.contains("?")) {
                linkStr = linkStr.substring(0, linkStr.indexOf("?"));
            }
            if (!urlQueue.contains(linkStr) && !visitedList.contains(linkStr)) {
                i++;
                urlQueue.add(linkStr);
            }
        }
        System.out.println(links.size() - i + " links are ignored.");
        System.out.println(i + " links are added to queue.");
    }

    private void checkWeatherCrawlingIsFinished() {
        if (urlQueue.size() == 0) {
            System.out.println("************Crawling is finished**************");
            System.out.println("All the gathered information are saved in products.db database");
        }
    }

    /**
     * Search through document and get the product details and returns in a Map
     *
     * @param document
     * @return
     */
    public Map<String, String> getProductInfo(Document document) {
        Map<String, String> productInfoMap = new HashMap<>();

        getTitle(document, productInfoMap);
        getPrice(document, productInfoMap);
        getDescription(document, productInfoMap);
        getExtraInformation(document, productInfoMap);

        return productInfoMap;
    }

    private void getTitle(Document document, Map<String, String> productInfoMap) {
        String title = document.title();
        productInfoMap.put(TITLE, title);
    }

    private void getPrice(Document document, Map<String, String> productInfoMap) {
        String price = document.select("div.product-info-main").select("div.product-info-price").select("span.price").text();
        productInfoMap.put(PRICE, price);
    }

    private void getDescription(Document document, Map<String, String> productInfoMap) {
        String description = document.select("div.product.attribute.description").text();
        productInfoMap.put(DESCRIPTION, description);
    }

    private void getExtraInformation(Document document, Map<String, String> productInfoMap) {
        StringBuilder extraInformation = new StringBuilder();
        String style = document.getElementById("product-attribute-specs-table").select("td[data-th=Style]").text();
        String material = document.getElementById("product-attribute-specs-table").select("td[data-th=Material]").text();
        String pattern = document.getElementById("product-attribute-specs-table").select("td[data-th=Pattern]").text();
        String climate = document.getElementById("product-attribute-specs-table").select("td[data-th=Climate]").text();

        extraInformation
                .append("Style  ").append(style)
                .append("\nMaterial  ").append(material)
                .append("\nPattern  ").append(pattern)
                .append("\nClimate  ").append(climate);

        productInfoMap.put(EXTRA_INFORMATION, extraInformation.toString());
    }

    private String connect(String url) throws IOException {
        StringBuilder stringBuilder = new StringBuilder();
        BufferedReader br = null;
        try {
            URL webURL = new URL(url);
            br = new BufferedReader(new InputStreamReader(webURL.openStream()));
            String line = "";
            while (null != (line = br.readLine())) {
                stringBuilder.append(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (br != null) {
                br.close();
            }
        }
        return stringBuilder.toString();
    }

    private Set<String> getLinks(String text) {
        Set<String> linkSet = new HashSet<>();
        String regex = "\\(?\\b(http:\\/\\/|www[.])[-A-Za-z0-9+&amp;@#%?=~_()|!:,.;]*[-A-Za-z0-9+&amp;@#%=~_()|]\\/[-A-Za-z0-9+&amp;@#%?=~_()|!:,.;]*[-A-Za-z0-9+&amp;@#%=~_()|].html";
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(text);
        while (matcher.find()) {
            String link = matcher.group();
            linkSet.add(link);
        }
        return linkSet;
    }
}