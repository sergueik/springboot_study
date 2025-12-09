package ge.vakho.native_messaging;

import com.fasterxml.jackson.databind.ObjectMapper;
import ge.vakho.native_messaging.protocol.NativeRequest;
import ge.vakho.native_messaging.protocol.NativeResponse;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import static org.junit.Assert.assertEquals;

public class NativeMessagingMockTest {

    private final ObjectMapper mapper = new ObjectMapper();

    @Test
    public void testLengthPrefixedMessage() throws IOException {
        // Prepare mock request
        NativeRequest request = new NativeRequest();
        request.setMessage("ping");
        String jsonRequest = mapper.writeValueAsString(request);
        byte[] payload = jsonRequest.getBytes("UTF-8");

        // Add 4-byte length prefix (little-endian, as Chrome expects)
        int len = payload.length;
        byte[] header = new byte[]{
                (byte) (len & 0xFF),
                (byte) ((len >> 8) & 0xFF),
                (byte) ((len >> 16) & 0xFF),
                (byte) ((len >> 24) & 0xFF)
        };

        // Combine header + payload into a byte stream
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        baos.write(header);
        baos.write(payload);
        byte[] fullMessage = baos.toByteArray();

        // Simulate input stream from Chrome
        ByteArrayInputStream input = new ByteArrayInputStream(fullMessage);

        // Read header
        byte[] readHeader = new byte[4];
        input.read(readHeader);
        int readLen = ((readHeader[3] & 0xFF) << 24) |
                      ((readHeader[2] & 0xFF) << 16) |
                      ((readHeader[1] & 0xFF) << 8) |
                      (readHeader[0] & 0xFF);

        // Read JSON payload
        byte[] readPayload = new byte[readLen];
        int read = input.read(readPayload);
        assertEquals(readLen, read);

        String readJson = new String(readPayload, "UTF-8");

        // Deserialize
        NativeRequest deserialized = mapper.readValue(readJson, NativeRequest.class);
        assertEquals("ping", deserialized.getMessage());

        // Prepare response
        NativeResponse response = new NativeResponse();
        response.setMessage("Hello, " + deserialized.getMessage() + "!");

        String jsonResponse = mapper.writeValueAsString(response);
        byte[] respPayload = jsonResponse.getBytes("UTF-8");

        // Prepend header
        byte[] respHeader = new byte[]{
                (byte) (respPayload.length & 0xFF),
                (byte) ((respPayload.length >> 8) & 0xFF),
                (byte) ((respPayload.length >> 16) & 0xFF),
                (byte) ((respPayload.length >> 24) & 0xFF)
        };

        ByteArrayOutputStream responseStream = new ByteArrayOutputStream();
        responseStream.write(respHeader);
        responseStream.write(respPayload);

        byte[] finalResponse = responseStream.toByteArray();

        // Check total length
        assertEquals(respPayload.length + 4, finalResponse.length);

        // Optional: deserialize response back
        ByteArrayInputStream responseInput = new ByteArrayInputStream(finalResponse);
        byte[] headerBack = new byte[4];
        responseInput.read(headerBack);
        int respLenBack = ((headerBack[3] & 0xFF) << 24) |
                          ((headerBack[2] & 0xFF) << 16) |
                          ((headerBack[1] & 0xFF) << 8) |
                          (headerBack[0] & 0xFF);

        byte[] payloadBack = new byte[respLenBack];
        responseInput.read(payloadBack);
        NativeResponse respDeserialized = mapper.readValue(payloadBack, NativeResponse.class);

        assertEquals("Hello, ping!", respDeserialized.getMessage());
    }
}

