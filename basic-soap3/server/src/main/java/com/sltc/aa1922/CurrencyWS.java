//AA1922
package com.sltc.aa1922;

import org.json.simple.parser.ParseException;

import javax.jws.WebMethod;
import javax.jws.WebService;
import javax.jws.soap.SOAPBinding;
import javax.xml.ws.Endpoint;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;

@WebService
@SOAPBinding(style = SOAPBinding.Style.DOCUMENT)
public class CurrencyWS {
	@WebMethod
	public double convert(double amountInSourceCurrency, String sourceCurrency,
			String targetCurrency) throws IOException, ParseException {

		ReadJson readJson = new ReadJson();

		Double sourceCurrencyRate = Double
				.valueOf(readJson.conversionRates.get(sourceCurrency).toString());
		Double targetCurrencyRate = Double
				.valueOf(readJson.conversionRates.get(targetCurrency).toString());

		return (amountInSourceCurrency / sourceCurrencyRate) * targetCurrencyRate;
	}

	@WebMethod
	public ArrayList getCurrencyList() throws IOException, ParseException {

		ReadJson readJson = new ReadJson();
		ArrayList currencyList = readJson.currencyList;
		Collections.sort(currencyList);
		return currencyList;
	}

	public static void main(String[] args) {

		Endpoint.publish("http://0.0.0.0:8888/CurrencyConversionWebServiceAA1922",
				new CurrencyWS());
	}
}
