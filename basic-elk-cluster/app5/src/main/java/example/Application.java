package example;

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
public class Application {
	private Utils utils;

	@WebMethod
	public double convert(Double amountInSourceCurrency, String sourceCurrency, String targetCurrency)
			throws IOException, ParseException {
		try {
			utils = new Utils();
			// NOTE: some rates data is read from JSON as long
			// This leads to an exception:
			/// java.lang.Long cannot be cast to
			// java.lang.Double
			// which becomes an zeep.exceptions.Fault
			Double sourceCurrencyRate = utils.conversionRates.get(sourceCurrency);
			Double targetCurrencyRate = utils.conversionRates.get(targetCurrency);
			return (amountInSourceCurrency / sourceCurrencyRate) * targetCurrencyRate;
		} catch (Exception e) {
			return 0;
		}
	}

	@WebMethod
	public ArrayList<String> getCurrencyList() throws IOException, ParseException {
		utils = new Utils();

		ArrayList<String> currencyList = utils.currencyList;
		Collections.sort(currencyList);
		return currencyList;
	}

	public static void main(String[] args) {

		Endpoint.publish("http://0.0.0.0:8888/CurrencyConversionWebService", new Application());
	}
}
