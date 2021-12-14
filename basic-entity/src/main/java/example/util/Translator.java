package example.util;

import example.util.Constants;

import java.util.Map;

import example.model.Product;

public class Translator {

	public static Product personTranslator(Map<String, String> productInfo) {
		Product product = new Product();
		product.setName(productInfo.get(Constants.TITLE));
		product.setPrice(productInfo.get(Constants.PRICE));
		product.setDescription(productInfo.get(Constants.DESCRIPTION));
		product.setExtraInformation(productInfo.get(Constants.EXTRA_INFORMATION));

		return product;
	}
}
