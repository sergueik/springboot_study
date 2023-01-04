package example;

import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;

import org.json.simple.JSONObject;
import org.json.simple.parser.*;

public class Utils {
	public String base;
	public HashMap<String, Double> conversionRates;
	public ArrayList<String> currencyList;

	@SuppressWarnings("unchecked")
	public Utils() throws IOException, ParseException {
		Object obj = new JSONParser()
				.parse(new FileReader("src/main/resources/conversionRates.json"));

		JSONObject data = (JSONObject) obj;

		base = (String) data.get("base");
		conversionRates = ((HashMap<String, Double>) data.get("rates"));
		currencyList = new ArrayList<>(conversionRates.keySet());
	}
}
