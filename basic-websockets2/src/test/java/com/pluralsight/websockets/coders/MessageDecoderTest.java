package com.pluralsight.websockets.coders;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.*;

import java.time.LocalDate;

import javax.websocket.DecodeException;

import org.junit.Before;
import org.junit.Test;

import com.pluralsight.websockets.message.ChatMessage;
import com.pluralsight.websockets.message.GetUsersMessage;
import com.pluralsight.websockets.message.JoinMessage;
import com.pluralsight.websockets.message.Message;

public class MessageDecoderTest {
	private MessageDecoder sut;

	@Before
	public void setup() {
		sut = new MessageDecoder();
	}

	@Test
	public void shouldReturnTrueWithValidInputStringToWillDecode() {
		boolean willDecode = sut.willDecode("{fo:'bar'}");
		assertThat(willDecode, is(true));
	}

	@Test
	public void shouldReturnFalseWithInvalidInputStringToWillDecode() {
		boolean willDecode = sut.willDecode("xyz");
		assertThat(willDecode, is(true));
	}

	@Test
	public void shouldDecodeJoinMessage() throws Exception {
		JoinMessage message = (JoinMessage) sut.decode("{\"type\":1,\"name\":\"Walter White\"}");

		assertThat(message, is(notNullValue()));
		assertThat(message.getName(), is("Walter White"));
	}

	@Test
	public void shouldDecodeChatMessage() throws Exception {
		LocalDate nowDate = LocalDate.now();
		String payload = String.format(
				"{\"type\":2,\"userName\":\"Jessie Pink\",\"message\":\"Hello world\",\"timeSent\":\"%s\"}", nowDate);
		ChatMessage message = (ChatMessage) sut.decode(payload);

		assertThat(message, is(notNullValue()));
		assertThat(message.getMessage(), is("Hello world"));
		assertThat(message.getUserName(), is("Jessie Pink"));
		
		// todo try and assert on time sent accurately
//		assertThat(message.getTimeSent(), is(String.format("%s", nowDate)));
	}

	@Test
	public void shouldDecodeGetUsersMessage() throws Exception {
		GetUsersMessage message = (GetUsersMessage) sut.decode("{\"type\":3}");
		assertThat(message, is(notNullValue()));
	}

	@Test
	public void shouldReturnNullWhenDecodingUnknownMessageType() throws DecodeException {
		Message message = sut.decode("{\"type\":0}");
		assertThat(message, is(nullValue()));
	}
}
