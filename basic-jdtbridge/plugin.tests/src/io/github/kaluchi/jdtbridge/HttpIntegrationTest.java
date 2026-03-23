package io.github.kaluchi.jdtbridge;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.Socket;
import java.nio.charset.StandardCharsets;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Tests the HTTP server layer: real TCP connections, auth, routing.
 */
public class HttpIntegrationTest {

    private static HttpServer server;
    private static int port;
    private static final String TOKEN = "test-secret-token-42";

    @BeforeClass
    public static void setUp() throws Exception {
        TestFixture.create();
        server = new HttpServer();
        server.setToken(TOKEN);
        server.start();
        port = server.getPort();
        assertTrue("Port should be assigned", port > 0);
    }

    @AfterClass
    public static void tearDown() throws Exception {
        if (server != null) server.stop();
        TestFixture.destroy();
    }

    // ---- Auth ----

    @Test
    public void noTokenReturns401() throws Exception {
        String response = rawRequest("GET /projects HTTP/1.1",
                "Host: localhost");
        assertTrue("Should be 401: " + response,
                response.startsWith("HTTP/1.1 401"));
    }

    @Test
    public void wrongTokenReturns401() throws Exception {
        String response = rawRequest("GET /projects HTTP/1.1",
                "Host: localhost",
                "Authorization: Bearer wrong-token");
        assertTrue("Should be 401: " + response,
                response.startsWith("HTTP/1.1 401"));
    }

    @Test
    public void correctTokenReturns200() throws Exception {
        String response = rawRequest("GET /projects HTTP/1.1",
                "Host: localhost",
                "Authorization: Bearer " + TOKEN);
        assertTrue("Should be 200: " + response,
                response.startsWith("HTTP/1.1 200"));
    }

    @Test
    public void basicSchemeReturns401() throws Exception {
        String response = rawRequest("GET /projects HTTP/1.1",
                "Host: localhost",
                "Authorization: Basic dXNlcjpwYXNz");
        assertTrue("Basic auth should be rejected: " + response,
                response.startsWith("HTTP/1.1 401"));
    }

    @Test
    public void bearerWithExtraSpacesReturns401() throws Exception {
        String response = rawRequest("GET /projects HTTP/1.1",
                "Host: localhost",
                "Authorization:  Bearer  " + TOKEN);
        // Extra spaces around Bearer — should fail (strict match)
        assertTrue("Malformed Bearer should be 401: " + response,
                response.startsWith("HTTP/1.1 401"));
    }

    @Test
    public void emptyAuthHeaderReturns401() throws Exception {
        String response = rawRequest("GET /projects HTTP/1.1",
                "Host: localhost",
                "Authorization: ");
        assertTrue("Empty auth should be 401: " + response,
                response.startsWith("HTTP/1.1 401"));
    }

    @Test
    public void authHeaderCaseInsensitive() throws Exception {
        // HTTP header names are case-insensitive (RFC 7230)
        String response = rawRequest("GET /projects HTTP/1.1",
                "Host: localhost",
                "authorization: Bearer " + TOKEN);
        assertTrue("Lowercase header should work: " + response,
                response.startsWith("HTTP/1.1 200"));
    }

    @Test
    public void serverWithoutTokenAllowsAll() throws Exception {
        // Start a second server without token
        HttpServer openServer = new HttpServer();
        openServer.start();
        int openPort = openServer.getPort();
        try {
            try (Socket socket = new Socket("localhost", openPort)) {
                socket.setSoTimeout(5000);
                OutputStream out = socket.getOutputStream();
                out.write("GET /projects HTTP/1.1\r\nHost: localhost\r\n\r\n"
                        .getBytes(StandardCharsets.UTF_8));
                out.flush();
                BufferedReader reader = new BufferedReader(
                        new InputStreamReader(socket.getInputStream(),
                                StandardCharsets.UTF_8));
                String firstLine = reader.readLine();
                assertTrue("No-token server should allow: " + firstLine,
                        firstLine.startsWith("HTTP/1.1 200"));
            }
        } finally {
            openServer.stop();
        }
    }

    @Test
    public void error401BodyIsJson() throws Exception {
        String response = rawRequest("GET /projects HTTP/1.1",
                "Host: localhost");
        assertTrue("401 body should be JSON: " + response,
                response.contains("{\"error\":\"Unauthorized\"}"));
    }

    // ---- Routing ----

    @Test
    public void projectsEndpoint() throws Exception {
        String body = authedGet("/projects");
        assertTrue("Should be JSON array: " + body,
                body.startsWith("["));
        assertTrue("Should include test project: " + body,
                body.contains(TestFixture.PROJECT_NAME));
    }

    @Test
    public void findEndpoint() throws Exception {
        String body = authedGet("/find?name=Dog&source");
        assertTrue("Should find Dog: " + body,
                body.contains("test.model.Dog"));
    }

    @Test
    public void unknownPathReturnsError() throws Exception {
        String body = authedGet("/nonexistent");
        assertTrue("Should be error: " + body,
                body.contains("Unknown path"));
    }

    @Test
    public void errorsEndpoint() throws Exception {
        String body = authedGet("/errors?project="
                + TestFixture.PROJECT_NAME + "&no-refresh");
        assertTrue("Should contain BrokenClass error: " + body,
                body.contains("BrokenClass"));
    }

    @Test
    public void typeInfoEndpoint() throws Exception {
        String body = authedGet("/type-info?class=test.model.Animal");
        assertTrue("Should be interface: " + body,
                body.contains("\"kind\":\"interface\""));
    }

    @Test
    public void sourceEndpoint() throws Exception {
        String response = rawRequest("GET /source?class=test.model.Dog HTTP/1.1",
                "Host: localhost",
                "Authorization: Bearer " + TOKEN);
        assertTrue("Should be 200: " + response,
                response.startsWith("HTTP/1.1 200"));
        assertTrue("Should have X-File header: " + response,
                response.contains("X-File:"));
        assertTrue("Should have source body: " + response,
                response.contains("public class Dog"));
    }

    @Test
    public void queryParamEncoding() throws Exception {
        // URL-encoded class name
        String body = authedGet(
                "/type-info?class=test.model.Dog");
        assertTrue("Should work with dots: " + body,
                body.contains("test.model.Dog"));
    }

    // ---- Helpers ----

    private String authedGet(String path) throws Exception {
        String response = rawRequest("GET " + path + " HTTP/1.1",
                "Host: localhost",
                "Authorization: Bearer " + TOKEN);
        // Extract body (after blank line)
        int bodyStart = response.indexOf("\r\n\r\n");
        if (bodyStart >= 0) {
            return response.substring(bodyStart + 4);
        }
        return response;
    }

    private String rawRequest(String requestLine, String... headers)
            throws Exception {
        try (Socket socket = new Socket("localhost", port)) {
            socket.setSoTimeout(5000);
            OutputStream out = socket.getOutputStream();
            StringBuilder req = new StringBuilder();
            req.append(requestLine).append("\r\n");
            for (String h : headers) {
                req.append(h).append("\r\n");
            }
            req.append("\r\n");
            out.write(req.toString().getBytes(StandardCharsets.UTF_8));
            out.flush();

            BufferedReader reader = new BufferedReader(
                    new InputStreamReader(
                            socket.getInputStream(), StandardCharsets.UTF_8));
            StringBuilder response = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                response.append(line).append("\r\n");
            }
            return response.toString();
        }
    }
}
