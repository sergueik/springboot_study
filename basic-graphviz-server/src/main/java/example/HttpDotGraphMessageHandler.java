package example;
/**
 * Copyright 2026 Serguei Kouzmine
 */

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.Locale;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.apache.http.HttpEntity;
import org.apache.http.HttpEntityEnclosingRequest;
import org.apache.http.HttpException;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.MethodNotSupportedException;

import org.apache.http.entity.StringEntity;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.FileEntity;
import org.apache.http.protocol.HttpContext;
import org.apache.http.protocol.HttpRequestHandler;
import org.apache.http.util.EntityUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

// origin: https://github.com/omerio/graphviz-server/blob/master/src/info/dawelbeit/graphviz/dot/HttpDotGraphMessageHandler.java

public class HttpDotGraphMessageHandler implements HttpRequestHandler {

	private static final Logger log = LoggerFactory.getLogger(HttpRequestHandler.class);

	public HttpDotGraphMessageHandler() {
		super();
	}

	@Override
	public void handle(final HttpRequest request, final HttpResponse response, final HttpContext context)
			throws HttpException, IOException {

		String method = request.getRequestLine().getMethod().toUpperCase(Locale.ENGLISH);

		if (!(Set.of("GET", "HEAD", "POST").contains(method))) {
			throw new MethodNotSupportedException(String.format("method %s not supported", method));
		}

		log.debug(request.toString());

		// Refuse all requests while GraphViz is warming up
		if (!GraphViz.isReady()) {
			log.warn("GraphViz not ready yet - refusing request: {}", request.getRequestLine());

			response.setStatusCode(HttpStatus.SC_SERVICE_UNAVAILABLE);
			response.setEntity(
					new StringEntity("GraphViz engine is warming up, please retry shortly\n", ContentType.TEXT_PLAIN));
			return;
		}

		if (request instanceof HttpEntityEnclosingRequest) {

			if ("/health".equals(request.getRequestLine().getUri())) {
				if (GraphViz.isReady()) {
					response.setStatusCode(HttpStatus.SC_OK);
					response.setEntity(new StringEntity("OK\n"));
				} else {
					response.setStatusCode(HttpStatus.SC_SERVICE_UNAVAILABLE);
					response.setEntity(new StringEntity("PROBLEM WARMING UP\n"));
				}
				return;
			}

			HttpEntity entity = ((HttpEntityEnclosingRequest) request).getEntity();
			byte[] entityContent = EntityUtils.toByteArray(entity);
			String payload = new String(entityContent, StandardCharsets.US_ASCII);

			log.info("Incoming entity content ({} bytes): {}", entityContent.length, payload);

			if (StringUtils.isNotBlank(payload) && GraphViz.isValidDotText(payload)) {
				log.info("valid dot content");

				// --- Future: extract output format from URL (svg/pdf/png) ---
				/*
				 * String target = request.getRequestLine().getUri();
				 * String format = (StringUtils.isNotBlank(target) ?
				 * StringUtils.remove(URLDecoder.decode(target, "UTF-8").trim().toLowerCase(),
				 * '/') : null);
				 *
				 * log.info("requesting graph format: {}", format);
				 */
				try {
					HttpEntity graph = generateGraph(payload);
					log.info("Responded with {} bytes graph", graph.getContentLength());
					response.setStatusCode(HttpStatus.SC_OK);
					response.setEntity(graph);
				} catch (IllegalStateException e) {
					log.error("Graphviz engine concurrency error", e);
					response.setStatusCode(HttpStatus.SC_SERVICE_UNAVAILABLE);
					response.setEntity(
							new StringEntity("Graphviz engine busy, try again later\n", ContentType.TEXT_PLAIN));
				} catch (Exception e) {
					log.error("Graphviz render failed", e);
					response.setStatusCode(HttpStatus.SC_INTERNAL_SERVER_ERROR);
					response.setEntity(new StringEntity("Graphviz render failed\n", ContentType.TEXT_PLAIN));
				}
			} else {
				response.setStatusCode(HttpStatus.SC_BAD_REQUEST);
				response.setEntity(new StringEntity(String.format("not a valid dot content: %s", payload), ContentType.TEXT_PLAIN) );
				log.info("not a valid dot content: {}", payload);
			}
		}
	}

	/**
	 * Currently only renders PNG output. formats possibly to support in the future
	 * : SVG, PDF
	 */
	private HttpEntity generateGraph(String dot) {

		GraphViz gv = new GraphViz();
		gv.readString(dot);
		log.info(gv.getDotSource());

		String graphType = "png";
		ContentType contentType = ContentType.IMAGE_PNG;

		/*
		 * if ("svg".equals(target)) { graphType = "svg"; contentType =
		 * ContentType.APPLICATION_SVG_XML; } else if ("pdf".equals(target)) { graphType
		 * = "pdf"; contentType = ContentType.create("application/pdf"); }
		 */

		File out = Paths.get(System.getProperty("java.io.tmpdir"), "graph." + graphType).toFile();

		gv.writeGraphToFile(gv.getGraph(gv.getDotSource()), out.getAbsolutePath());

		return new FileEntity(out, contentType);
	}
}
