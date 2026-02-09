package example;

import org.graalvm.polyglot.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

public class GraphvizGraalEngine implements AutoCloseable {

    private static final Logger log = LoggerFactory.getLogger(GraphvizGraalEngine.class);

    private final Context context;
    private final Value vizFunction;

    public GraphvizGraalEngine() {
        log.info("Initializing GraalJS context...");
        context = Context.newBuilder("js")
                .allowAllAccess(true)
                .option("engine.WarnInterpreterOnly", "false")
                .build();
        log.info("GraalJS context created.");

        // Load Viz.js + full.render.js
        String vizJs = loadResource("/META-INF/resources/webjars/viz.js-graphviz-java/2.1.3/viz.js");
        log.info("Loaded viz.js, length={}", vizJs.length());
        context.eval("js", vizJs);

        String fullRenderJs = loadResource("/META-INF/resources/webjars/viz.js-graphviz-java/2.1.3/full.render.js");
        log.info("Loaded full.render.js, length={}", fullRenderJs.length());
        context.eval("js", fullRenderJs);

        // Grab Viz constructor
        vizFunction = context.getBindings("js").getMember("Viz");
        if (vizFunction == null || !vizFunction.canInstantiate()) {
            throw new IllegalStateException("Viz constructor not found or not instantiable");
        }
        log.info("Viz function found and executable.");
    }

    /** Render DOT source to PNG bytes */
    public byte[] renderPng(String dotSource) {
    	log.info("in renderPng");
        if (context == null || vizFunction == null) {
            throw new IllegalStateException("GraalJS context or Viz not initialized");
        }

        try {
            String jsWrapper = """
                (function(dot) {
                    var viz = new Viz();
                    var done = false;
                    var resultBase64 = null;
                    viz.renderString(dot, { format: 'png-image' })
                        .then(function(p) {
                            var arr = new Uint8Array(p);
                            resultBase64 = Array.from(arr).map(b => String.fromCharCode(b)).join('');
                            done = true;
                        })
                        .catch(function(e) { throw e; });
                    while(!done) {}
                    return resultBase64;
                })
                """;
            log.info("about to eval {}",jsWrapper);
                
            Value fn = context.eval("js", jsWrapper);
            log.info("about to send source {}",dotSource);
            String resultStr = fn.execute(dotSource).asString();
            log.info("received result {} bytes",resultStr.length());

            byte[] bytes = new byte[resultStr.length()];
            for (int i = 0; i < resultStr.length(); i++) {
                bytes[i] = (byte) resultStr.charAt(i);
            }

            log.info("Successfully rendered PNG, {} bytes", bytes.length);
            return bytes;

        } catch (PolyglotException e) {
            log.error("Exception while rendering PNG: {}", e.getMessage(), e);
            throw new RuntimeException("Error rendering PNG: " + e.getMessage(), e);
        }
    }

    private String loadResource(String path) {
        try (InputStream is = getClass().getResourceAsStream(path)) {
            if (is == null) throw new RuntimeException("Resource not found: " + path);
            return new String(is.readAllBytes(), StandardCharsets.UTF_8);
        } catch (Exception e) {
            throw new RuntimeException("Failed to load resource " + path, e);
        }
    }

    @Override
    public void close() {
        if (context != null) context.close();
    }
}
