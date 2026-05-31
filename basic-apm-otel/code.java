
import io.opentelemetry.api.OpenTelemetry;
import io.opentelemetry.api.trace.Span;
import io.opentelemetry.api.trace.Tracer;
import io.opentelemetry.context.Context;
import io.opentelemetry.context.propagation.TextMapGetter;
import io.opentelemetry.context.propagation.TextMapSetter;
import io.opentelemetry.extension.tracing.propagation.TraceContextPropagator;

OpenTelemetry openTelemetry = OpenTelemetrySdk.builder()
    .setTracerProvider(SdkTracerProvider.builder()
        .addSpanProcessor(SimpleSpanProcessor.create(new OTLPHttpSpanExporter()))
        .build())
    .setPropagators(ContextPropagators.create(TraceContextPropagator.getInstance()))
    .buildAndRegisterGlobal();

Tracer tracer = openTelemetry.getTracer("com.example.api-gateway");

