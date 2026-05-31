https://chatgpt.com/c/689b5ef5-6270-8320-afc6-41902be1144

 provides a minimal API Gateway in Java that:

Handles incoming requests on port 8080.

Extracts and injects W3C traceparent headers.

Forwards requests to backend services.

Sends trace data to a monitoring backend.

Serves static content with trace context.

the static page  itself must take care of keepoing the t race data to a monitoring backend:


alternatively gateway to do it via
```java
import io.opentelemetry.exporter.otlp.http.OTLPHttpSpanExporter;

OTLPHttpSpanExporter exporter = OTLPHttpSpanExporter.builder()
    .setEndpoint("http://your-otel-collector:4318/v1/traces")
    .build();


```
