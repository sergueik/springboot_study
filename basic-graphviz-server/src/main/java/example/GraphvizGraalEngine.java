package example;

import org.graalvm.polyglot.*;
import org.graalvm.polyglot.proxy.ProxyExecutable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.atomic.AtomicReference;

import org.apache.batik.transcoder.*;
import org.apache.batik.transcoder.image.PNGTranscoder;

public class GraphvizGraalEngine implements AutoCloseable {

    private static final Logger log = LoggerFactory.getLogger(GraphvizGraalEngine.class);

    private final Context context;

    public GraphvizGraalEngine() {
        try {
            log.info("Initializing GraalJS context...");

            context = Context.newBuilder("js")
                    .allowAllAccess(true)
                    .option("engine.WarnInterpreterOnly", "false")
                    .build();

            loadJsResource(Config.get("libraryPath", "/META-INF/resources/webjars/viz.js-graphviz-java/2.1.3") + "/viz.js");
            loadJsResource(Config.get("libraryPath", "/META-INF/resources/webjars/viz.js-graphviz-java/2.1.3") + "/full.render.js");

            log.info("Viz.js loaded successfully.");

        } catch (Exception e) {
            log.error("Failed to initialize Viz.js in GraalJS", e);
            throw new RuntimeException(e);
        }
    }

    private void loadJsResource(String path) throws IOException {
        log.info("Loading {}", path);

        try (InputStream is = getClass().getResourceAsStream(path)) {
            if (is == null) {
                throw new RuntimeException("Resource not found: " + path);
            }

            String js = new String(is.readAllBytes(), StandardCharsets.UTF_8);
            context.eval("js", js);

            log.info("Loaded {}, length={}", path, js.length());
        }
    }

    /**
     * Render DOT -> PNG
     */
    // NOTE :java.lang.IllegalStateException: 
    /// Multi threaded access requested by thread Thread[Thread-2,5,main] but is not allowed for language(s) js.
    public byte[] renderPng(String dotSource) {
        try {
            String js = """
            function renderSvg(dot) {
                var viz = new Viz({
                    Module: globalThis.Module,
                    render: globalThis.render
                });
                return viz.renderString(dot, { format: "svg" });
            }
            renderSvg
            """;

            Value renderFunc = context.eval("js", js);

            Value promise = renderFunc.execute(dotSource);

            String svg = awaitPromise(promise);

            log.info("SVG generated, length={}", svg.length());

            return svgToPng(svg);

        } catch (Exception e) {
            log.error("Rendering failed", e);
            throw new RuntimeException("Error converting SVG to PNG", e);
        }
    }

    /**
     * Synchronously await a JS Promise from GraalJS
     */
    private String awaitPromise(Value promise) {
        if (!promise.hasMember("then")) {
            throw new RuntimeException("Expected Promise, got: " + promise);
        }

        AtomicReference<String> result = new AtomicReference<>();
        AtomicReference<Throwable> error = new AtomicReference<>();

        promise.invokeMember("then",
                (ProxyExecutable) args -> {
                    result.set(args[0].asString());
                    return null;
                },
                (ProxyExecutable) args -> {
                    error.set(new RuntimeException(args[0].toString()));
                    return null;
                }
        );

        if (error.get() != null) {
            throw new RuntimeException("JS Promise failed", error.get());
        }

        return result.get();
    }

    /**
     * Convert SVG to PNG using Batik
     */
    private byte[] svgToPng(String svg) {
        try {
            // 🔧 Batik workaround: transparent is illegal for stroke
            svg = svg.replaceAll("stroke=\"transparent\"", "stroke=\"none\"");

            PNGTranscoder transcoder = new PNGTranscoder();

            TranscoderInput input = new TranscoderInput(new StringReader(svg));

            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            TranscoderOutput output = new TranscoderOutput(baos);

            transcoder.transcode(input, output);
            baos.flush();

            return baos.toByteArray();
        } catch (Exception e) {
            throw new RuntimeException("Error converting SVG to PNG", e);
        }
    }

    @Override
    public void close() {
        context.close();
    }
}
