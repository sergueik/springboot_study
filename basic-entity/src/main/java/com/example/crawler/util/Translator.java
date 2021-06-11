package com.example.crawler.util;

import com.example.crawler.model.Product;

import java.util.Map;

import static com.example.crawler.util.Constants.*;

public class Translator {

    public static Product personTranslator(Map<String, String> productInfo) {
        Product product = new Product();
        product.setName(productInfo.get(TITLE));
        product.setPrice(productInfo.get(PRICE));
        product.setDescription(productInfo.get(DESCRIPTION));
        product.setExtraInformation(productInfo.get(EXTRA_INFORMATION));

        return product;
    }
}
