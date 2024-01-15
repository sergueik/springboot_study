package ch.qos.logback.ext.logzio;

import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClientBuilder;

import java.io.IOException;

public class LogzioAppnder<E> extends AbstractLogzioAppnder<E> {

	private static final String USER_AGENT = "Mozilla/5.0";

	@Override
	protected void append(E eventObject) {
		String msg = this.layout.doLayout(eventObject);
		System.err.println(String.format("Sending: %s", msg));
		doPost(msg);
	}

	private void doPost(final String event) {
		try {

			HttpClient client = HttpClientBuilder.create().build();
			HttpPost post = new HttpPost(getEndpoint());

			// add header
			post.setHeader("User-Agent", USER_AGENT);
			post.setEntity(
					new StringEntity(event, ContentType.create(layout.getContentType())));

			HttpResponse response = null;

			response = client.execute(post);
			int responseCode = response.getStatusLine().getStatusCode();

			if (response.getStatusLine().getStatusCode() != 200) {
				String message = readResponseBody(response);
				addError(
						"Post to Logz.io failed. (HTTP " + responseCode + ").\n Endpoint:\n"
								+ getEndpoint() + "\nResponse body:\n" + message);
			}

		} catch (IOException e) {
			addError(
					"IOException occurred while attempting to communicate with Logz.io",
					e);
		}
	}

}
