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

//origin: https://github.com/omerio/graphviz-server/blob/master/src/info/dawelbeit/graphviz/dot/HttpDotGraphMessageHandler.java

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

		log.debug("Incoming request: {}", request.getRequestLine());

		// Refuse all requests while GraphViz is warming up
		if (!GraphViz.isReady()) {
			log.warn("GraphViz not ready yet - refusing request: {}", request.getRequestLine());
			response.setStatusCode(HttpStatus.SC_SERVICE_UNAVAILABLE);
			response.setEntity(new StringEntity("GraphViz engine is not ready\n", ContentType.TEXT_PLAIN));
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
			log.debug("Incoming entity content ({} bytes): {}", entityContent.length, payload);

			if (StringUtils.isNotBlank(payload) && GraphViz.isValidDotText(payload)) {
				log.debug("valid dot content");

				String format = resolveFormat(request);
				log.debug("processing request: format={}", format);

				if (format == null || !(Set.of("png", "svg").contains(format))) {
					log.info("rejecting request: format={}", format);

					// !!! CAREFUL HERE !!!
					// A common mistake: writing
					// "Unsupported format: " + format == null ? format : "<null>"
					// due to operator precedence
					// Java interprets this as ("Unsupported format: " + format) == null ? format : "<null>"
					// which passes null to StringEntity -> IllegalArgumentException if the format is null
					// This would lead to the subtle null crash that can be very hard to debug in a running thread.
					String message = "Unsupported format: " + (format != null ? format : "<null>");
					response.setStatusCode(HttpStatus.SC_BAD_REQUEST);
					response.setEntity(new StringEntity(message, ContentType.TEXT_PLAIN));
					return;
				}

				log.debug("requesting graph format: {}", format);
				try {
					HttpEntity graph = generateGraph(payload, format);
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
				String message = String.format("not a valid dot content: %s", payload);
				response.setEntity(new StringEntity(message, ContentType.TEXT_PLAIN));
				log.debug(message);
			}
		}
	}

	private String resolveFormat(HttpRequest request) {

		// NOTE: request.getParams() is legacy HttpCore (pre-4.3) and does not parse query parameters.
		// We therefore do NOT use request.getParams().getParameter("format") and parse everything manually
 
		String uri = request.getRequestLine().getUri();
		String path = uri;
		String query = null;

		int q = uri.indexOf('?');
		if (q >= 0) {
			path = uri.substring(0, q);
			query = uri.substring(q + 1);
		}

		String format = null;

		// 1. Query parameter takes priority
		if (query != null) {
			for (String pair : query.split("&")) {
				String[] kv = pair.split("=", 2);
				if (kv.length == 2 && kv[0].equalsIgnoreCase("format")) {
					format = kv[1].toLowerCase();
					break;
				}
			}

			if (format != null) {
				log.debug("Resolved format '{}' for uri '{}'", format, uri);
				return format;
			}
		}

		// 2. Path-based routing: only accept specific "paths"
		if ("/svg".equals(path)) {
			return "svg";
		}

		if ("/".equals(path) || path.isEmpty()) {
			return "png";
		}

		// 3. Any other path is invalid, discarded
		return null;
	}

	/**
	 * Currently only renders PNG or SVG output
	 */
	private HttpEntity generateGraph(String dot) {
		return generateGraph(dot, "png");
	}

	private HttpEntity generateGraph(String dot, String format) {

		GraphViz gv = new GraphViz();
		gv.readString(dot);
		log.debug("DOT source:\n{}", gv.getDotSource());

		ContentType contentType = "svg".equals(format) ? ContentType.APPLICATION_SVG_XML : ContentType.IMAGE_PNG;

		File out = Paths.get(System.getProperty("java.io.tmpdir"), "graph." + format).toFile();

		gv.writeGraphToFile(gv.getGraph(gv.getDotSource(), format), out.getAbsolutePath());

		return new FileEntity(out, contentType);
	}
}
