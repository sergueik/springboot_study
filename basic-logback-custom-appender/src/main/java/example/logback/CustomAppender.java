package example.logback;

import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClientBuilder;

import java.io.IOException;

public class CustomAppender<E> extends AbstractCustomAppender<E> {

	private static final String USER_AGENT = "Mozilla/5.0";

	@Override
	protected void append(E eventObject) {
		final String message = this.layout.doLayout(eventObject);
		System.err.println(String.format("Sending: %s", message));
		doPost(message);
	}

	private void doPost(final String event) {
		try {

			HttpClient httpClient = HttpClientBuilder.create().build();
			HttpPost httpPost = new HttpPost(getEndpoint());

			// add header
			httpPost.setHeader("User-Agent", USER_AGENT);
			httpPost.setEntity(new StringEntity(event, ContentType.create(layout.getContentType())));

			// https://hc.apache.org/httpcomponents-client-4.5.x/current/httpclient/apidocs/org/apache/http/client/HttpClient.html
			HttpResponse httpResponse =  httpClient.execute(httpPost);
			int statusCode = httpResponse.getStatusLine().getStatusCode();
			if (statusCode != 200) {
				String message = readResponseBody(httpResponse);
				addError("Post to Log rest server failed. (HTTP STATUS: " + statusCode + ").\n Endpoint:\n"
						+ getEndpoint() + "\nResponse body:\n" + message);
			}

		} catch (IOException e) {
			addError("IOException occurred communicating Log rest server: ", e);
		}
	}

}
