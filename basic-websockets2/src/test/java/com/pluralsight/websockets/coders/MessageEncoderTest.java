package com.pluralsight.websockets.coders;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.greaterThan;
import static org.junit.Assert.assertThat;

import org.junit.Before;
import org.junit.Test;

import com.pluralsight.websockets.message.Message;

public class MessageEncoderTest {

	private MessageEncoder sut;

	@Before
	public void setup() {
		sut = new MessageEncoder();
	}
	
	@Test
	public void shouldEncodeMessage() throws Exception {
		Message message = new Message();
		message.setType(1);
		String encodedMessage = sut.encode(message);
		
		assertThat(encodedMessage, is(notNullValue()));
		assertThat(encodedMessage.length(), greaterThan(3));
		assertThat(encodedMessage, containsString("\"type\":1"));
	}
}
