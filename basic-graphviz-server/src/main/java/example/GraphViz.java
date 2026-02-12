package example;
/**
 * Copyright 2026 Serguei Kouzmine
 */

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.ByteArrayOutputStream;
import java.io.InputStreamReader;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import example.GraphvizGraalEngine;
import example.Config;

// based on: https://github.com/nidi3/graphviz-java/blob/master/graphviz-java/src/main/java/guru/nidi/graphviz/engine/Graphviz.java

public class GraphViz {

	private static final Logger log = LoggerFactory.getLogger(GraphViz.class);
	private static volatile boolean ready = false;
	private static GraphvizGraalEngine graalEngine;

	public static void init() {
		String engine = Config.get("graphviz.engine", "jdk");
		log.info("Using Graphviz engine: {}", engine);

		graalEngine = new GraphvizGraalEngine(); // initialize GraalJS engine
		if (graalEngine != null)
			log.info("Graphviz engine, {} created", graalEngine.hashCode());
		else
			log.info("Graphviz engine failed to initialize");

		// optional warmup
		String warmupDot = "graph { a -- b }";
		byte[] warmup = graalEngine.renderPng(warmupDot);
		log.info("Graphviz engine warmup complete, {} bytes generated", warmup.length);

		ready = true;
	}

	public static boolean isReady() {
		return ready;
	}

	private StringBuilder graph = new StringBuilder();
	// private static final String TEMP_DIR = System.getProperty("java.io.tmpdir");

	public GraphViz() {
	}
	public String getDotSource() {
		return graph.toString();
	}

	public void readString(String dot) {
		this.graph = new StringBuilder(dot);
	}

	public byte[] getGraph(String dotSource) {
		try {
			return graalEngine.renderPng(dotSource);
		} catch (Exception e) {
			log.error("Failed to render graph", e);
			return null;
		}
	}

	public int writeGraphToFile(byte[] img, String filePath) {
		if (img == null)
			return -1;
		try (FileOutputStream fos = new FileOutputStream(new File(filePath))) {
			fos.write(img);
		} catch (IOException e) {
			log.error("Error writing image to file", e);
			return -1;
		}
		return 1;
	}

	public static boolean isValidDotText(String dot) {
		// DOT grammar (simplified):
		// [ strict ] (graph | digraph) [ ID ] { ... }
	    return !StringUtils.isBlank(dot)
	        && Pattern.compile(
	            "(?x)                       # enable free-spacing & comments\n" +
	            "^\\s*                      # leading whitespace\n" +
	            "(strict\\s+)?              # optional 'strict'\n" +
	            "(graph|digraph)\\s+        # graph type\n" +
	            "([A-Za-z_][A-Za-z0-9_]*    # unquoted graph name\n" +
	            "|\"[^\"]+\")?              # or quoted graph name\n" +
	            "\\s*\\{                    # opening brace\n",
	            Pattern.CASE_INSENSITIVE | Pattern.COMMENTS
	        ).matcher(dot).find()
	        && dot.indexOf('{') < dot.lastIndexOf('}');
	}

}
