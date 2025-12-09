package ge.vakho.native_messaging.main;

import java.io.IOException;

import java.io.InputStream;
import java.io.InterruptedIOException;

import com.fasterxml.jackson.databind.ObjectMapper;

import ge.vakho.native_messaging.protocol.NativeRequest;
import ge.vakho.native_messaging.protocol.NativeResponse;

import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.ByteOrder;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class Main {

	private final static Logger logger = LoggerFactory.getLogger(Main.class);

	public static void main(String[] args) throws Exception {

		// Read message
		String requestJson = readMessage(System.in);
		ObjectMapper mapper = new ObjectMapper();
		NativeRequest request = null;
		try {
			request = mapper.readValue(requestJson, NativeRequest.class);
		} catch (Exception e) {
			logger.info(String.format("exception %s", e.toString()));
		}
		logger.info(String.format("Native Request JSON %s", request));

		// Process request...
		NativeResponse response = new NativeResponse();
		response.setMessage("Hello, " + request.getMessage() + "!");

		// Send response message back
		String responseJson = mapper.writeValueAsString(response);
		sendMessage(responseJson);
		logger.info(String.format("Response JSON %s", responseJson));

		System.exit(0);
	}

	private static String readMessage(InputStream in) throws IOException {
		byte[] b = new byte[4];
		in.read(b); // Read the size of message

		int size = getInt(b);

		if (size == 0) {
			throw new InterruptedIOException("Blocked communication");
		}

		b = new byte[size];
		in.read(b);

		return new String(b, "UTF-8");
	}

	private static void sendMessage(String message) throws IOException {

		// Convert the message string to UTF-8 bytes
		byte[] messageBytes = message.getBytes(StandardCharsets.UTF_8);
		byte[] lengthPrefix = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putInt(messageBytes.length).array();

		logger.info("Length prefix bytes: " + bytesToHex(lengthPrefix));
		logger.info("Payload bytes: " + bytesToHex(messageBytes));

		try (DataOutputStream out = new DataOutputStream(System.out)) {
			out.writeInt(Integer.reverseBytes(messageBytes.length));

			out.write(messageBytes);

			out.flush();
		}

		/*
		 * System.out.write(getBytes(message.length()));
		 * System.out.write(message.getBytes("UTF-8")); System.out.flush();
		 */
	}

	public static int getInt(byte[] bytes) {
		return (bytes[3] << 24) & 0xff000000 | (bytes[2] << 16) & 0x00ff0000 | (bytes[1] << 8) & 0x0000ff00
				| (bytes[0] << 0) & 0x000000ff;
	}

	public static byte[] getBytes(int length) {
		byte[] bytes = new byte[4];
		bytes[0] = (byte) (length & 0xFF);
		bytes[1] = (byte) ((length >> 8) & 0xFF);
		bytes[2] = (byte) ((length >> 16) & 0xFF);
		bytes[3] = (byte) ((length >> 24) & 0xFF);
		return bytes;
	}

	private static String bytesToHex(byte[] bytes) {
		StringBuilder sb = new StringBuilder();
		for (byte b : bytes) {
			sb.append(String.format("%02X ", b));
		}
		return sb.toString();
	}
}
