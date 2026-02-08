package example;

import org.graalvm.polyglot.*;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;

import org.apache.batik.transcoder.*;
import org.apache.batik.transcoder.image.PNGTranscoder;
import org.apache.batik.transcoder.TranscoderInput;
import org.apache.batik.transcoder.TranscoderOutput;

public class GraphvizGraalEngine {

	private final Context context;
	private final Value vizFunction;

	public GraphvizGraalEngine() {
		context = Context.newBuilder("js").allowAllAccess(true).option("engine.WarnInterpreterOnly", "false").build();
		String basePath = "/META-INF/resources/webjars/viz.js-graphviz-java/";
		String vizJs = loadResource(basePath + "2.1.3/" + "viz.js");
		context.eval("js", vizJs);

		vizFunction = context.getBindings("js").getMember("Viz");
		if (vizFunction == null || !vizFunction.canExecute()) {
			throw new IllegalStateException("Viz function not found in viz.js");
		}
	}

	/** Render DOT source to PNG bytes using Batik */
	public byte[] renderPng(String dotSource) {
		try {
			// Execute viz.js to get SVG string
			String svg = vizFunction.execute(dotSource).asString();

			// Transcode SVG → PNG
			try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
				Transcoder transcoder = new PNGTranscoder();
				TranscoderInput input = new TranscoderInput(new StringReader(svg));
				TranscoderOutput output = new TranscoderOutput(baos);
				transcoder.transcode(input, output);
				return baos.toByteArray();
			}
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	private String loadResource(String path) {
		try (InputStream is = getClass().getResourceAsStream(path)) {
			if (is == null)
				throw new RuntimeException("Resource not found: " + path);
			return new String(is.readAllBytes(), StandardCharsets.UTF_8);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	public void close() {
		context.close();
	}
}
