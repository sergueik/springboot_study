package ge.vakho.native_messaging;

import ge.vakho.native_messaging.protocol.NativeResponse;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.Test;
import static org.junit.Assert.*;

public class MainTest {

	@Test
	public void testResponse() throws Exception {
		NativeResponse resp = new NativeResponse();
		resp.setMessage("Hello, test!");
		assertEquals("Hello, test!", resp.getMessage());
	}

	@Test
	public void testPingCommand() throws Exception {
		String json = "{\"message\":\"Hello, mock!\"}";
		ObjectMapper mapper = new ObjectMapper();
		NativeResponse response = mapper.readValue(json, NativeResponse.class);
		assertEquals("Hello, mock!", response.getMessage());
	}
}
