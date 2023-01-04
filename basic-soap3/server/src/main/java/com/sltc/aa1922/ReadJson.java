package com.sltc.aa1922;

import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;

import org.json.simple.JSONObject;
import org.json.simple.parser.*;

public class ReadJson {
	public String base;
	public HashMap conversionRates;
	public ArrayList<String> currencyList;

	public ReadJson() throws IOException, ParseException {
		Object obj = new JSONParser()
				.parse(new FileReader("src/main/resources/conversionRates.json"));

		JSONObject jo = (JSONObject) obj;

		base = (String) jo.get("base");
		conversionRates = ((HashMap) jo.get("rates"));

		Set<String> keySet = conversionRates.keySet();
		currencyList = new ArrayList<>(keySet);
	}
}
