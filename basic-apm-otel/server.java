import com.sun.net.httpserver.HttpServer;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpExchange;

import java.net.InetSocketAddress;
import java.io.IOException;
import java.io.OutputStream;

public class ApiGatewayServer {
    public static void main(String[] args) throws IOException {
        HttpServer server = HttpServer.create(new InetSocketAddress(8080), 0);
        server.createContext("/api", new ApiHandler());
        server.createContext("/", new StaticContentHandler());
        server.start();
    }

    static class ApiHandler implements HttpHandler {
        public void handle(HttpExchange exchange) throws IOException {
            // Extract traceparent from request headers
            String traceparent = exchange.getRequestHeaders().getFirst("traceparent");
            if (traceparent != null) {
                // Start a new span with the extracted traceparent
                Span span = tracer.spanBuilder("API Request")
                    .setParent(Context.current().with(TraceContextPropagator.getInstance().extract(Context.current(), traceparent, getter)))
                    .startSpan();
                try (Scope scope = span.makeCurrent()) {
                    // Forward request to backend service
                    // (Implementation depends on your backend setup)
                } finally {
                    span.end();
                }
            }

            // Send response
            String response = "API Gateway Response";
            exchange.sendResponseHeaders(200, response.getBytes().length);
            OutputStream os = exchange.getResponseBody();
            os.write(response.getBytes());
            os.close();
        }
    }

    static class StaticContentHandler implements HttpHandler {
        public void handle(HttpExchange exchange) throws IOException {
            // Serve static content (e.g., HTML files)
            // Inject traceparent header into response
            exchange.getResponseHeaders().set("traceparent", "00-..."); // Generate or extract traceparent
            String response = "<html><body>Static Content</body></html>";
            exchange.sendResponseHeaders(200, response.getBytes().length);
            OutputStream os = exchange.getResponseBody();
            os.write(response.getBytes());
            os.close();
        }
    }
}

