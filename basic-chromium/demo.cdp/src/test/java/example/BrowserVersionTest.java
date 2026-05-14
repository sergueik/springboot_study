package example;
/**
 * Copyright 2020,2021,2023 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.IOException;
import java.util.Arrays;

import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import com.neovisionaries.ws.client.WebSocketException;

import example.messaging.CDPClient.MessageTimeOutException;
import example.messaging.MessageBuilder;

public class BrowserVersionTest extends BaseTest {
	private JSONObject result = null;

	@Before
	public void beforeTest() throws IOException {
		super.setHeadless(true);
		super.beforeTest();
	}

	@Test
	public void test() {
		// Arrange
		try {
			CDPClient.setDebug(true);
			id = utils.getDynamicID();
			// Act
			CDPClient.sendMessage(MessageBuilder.buildBrowserVersionMessage(id));
			// Assert
			result = new JSONObject(CDPClient.getResponseMessage(id, null));
			System.err.println("Get Broswer Version: " + result);
			for (String field : Arrays.asList(new String[] { "protocolVersion",
					"product", "revision", "userAgent", "jsVersion" })) {
				assertThat("Expect the key:" + field, result.has(field), is(true));
			}

		} catch (JSONException | InterruptedException | MessageTimeOutException
				| IOException | WebSocketException e) {
			System.err.println("Exception (ignored): " + e.toString());
		}
	}
}
