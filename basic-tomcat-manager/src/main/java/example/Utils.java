package example;

import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import javax.servlet.http.HttpServletRequest;

// based on: https://mkyong.com/java/how-to-get-http-request-header-in-java/
@SuppressWarnings("serial")
public class Utils {
	public static Map<String, String> getHeadersInfo(HttpServletRequest request) {

		Map<String, String> map = new HashMap<>();

		Enumeration<String> headerNames = request.getHeaderNames();
		while (headerNames.hasMoreElements()) {
			String key = (String) headerNames.nextElement();
			String value = request.getHeader(key);
			map.put(key, value);
		}
		return map;
	}

	public static String printHeadersInfo(Map<String, String> headers) {

		final StringBuffer result = new StringBuffer();
		result.append("<table>");

		for (Entry<String, String> entry : headers.entrySet()) {
			String header = entry.getKey();
			String headerValue = entry.getValue();
			result.append("<tr>");
			result.append("<td>");
			result.append(header);
			result.append("</td>");
			result.append("<td>");
			result.append(headerValue);
			result.append("</td>");
			result.append("</tr>");
		}
		result.append("</table>");
		return result.toString();
	}

	private static String getUserAgent(HttpServletRequest request) {
		return request.getHeader("user-agent");
	}
}
