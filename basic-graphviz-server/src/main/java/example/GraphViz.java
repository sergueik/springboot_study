package example;

import guru.nidi.graphviz.engine.*;
import guru.nidi.graphviz.model.*;
import guru.nidi.graphviz.parse.Parser;

import java.io.*;
import java.io.IOException;
import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.InputStreamReader;

import org.apache.commons.lang3.StringUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class GraphViz {

	private static final Logger log = LoggerFactory.getLogger(GraphViz.class);

	private static volatile boolean ready = false;

	public static void init() {
		try {
			log.info("Initializing Graphviz engine...");
			Graphviz.useEngine(new GraphvizJdkEngine());

			// warmup render
			String warmupDot = "graph { a -- b }";
			MutableGraph g = new Parser().read(warmupDot);
			Graphviz.fromGraph(g).render(Format.PNG).toImage();

			ready = true;
			log.info("Graphviz engine warmup complete.");
		} catch (Exception e) {
			log.error("Graphviz warmup failed", e);
			ready = false;
		}
	}

	public static boolean isReady() {
		return ready;
	}

	private static String TEMP_DIR = "/tmp"; // Linux
	// private static String TEMP_DIR = "c:/temp"; // Windows

	// public static final String GRAPH_START = "digraph G {";
	public static final String GRAPH_START = "graph {";

	public static final String GRAPH_END = "}";

	private StringBuilder graph = new StringBuilder();

	public GraphViz() {
	}

	public String getDotSource() {
		return graph.toString();
	}

	public void add(String line) {
		graph.append(line);
	}

	public void addln(String line) {
		graph.append(line + "\n");
	}

	public void addln() {
		graph.append('\n');
	}

	public byte[] getGraph(String dot_source, String type) {
		File dot;
		byte[] img_stream = null;

		try {
			dot = writeDotSourceToFile(dot_source);
			if (dot != null) {
				img_stream = get_img_stream(dot, type);
				if (dot.delete() == false)
					log.warn("Warning: " + dot.getAbsolutePath() + " could not be deleted!");
				return img_stream;
			}
			return null;
		} catch (IOException e) {
			log.error("Exdeption processing: {}", e.toString(), e);
			return null;
		} catch (Exception e) {
			log.error("Exdeption processing: {}", e.toString(), e);
			return null;
		}

	}

	public int writeGraphToFile(byte[] img, String file) {
		File to = new File(file);
		return writeGraphToFile(img, to);
	}

	public int writeGraphToFile(byte[] img, File to) {
		try {
			FileOutputStream fos = new FileOutputStream(to);
			fos.write(img);
			fos.close();
		} catch (java.io.IOException ioe) {
			return -1;
		}
		return 1;
	}

	public byte[] get_img_stream(File dotFile, String type) throws Exception {
		log.info("Rendering graph {} using graphviz-java engine, format={}", dotFile.toString(), type);

		MutableGraph g;
		try (FileInputStream fis = new FileInputStream(dotFile)) {
			g = new Parser().read(fis);
		}

		Format format = Format.valueOf(type.toUpperCase());

		try (ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream()) {
			Graphviz.fromGraph(g).render(format).toOutputStream(byteArrayOutputStream);

			return byteArrayOutputStream.toByteArray();
		}
	}

	private File writeDotSourceToFile(String str) throws IOException {
		File temp;
		try {
			temp = File.createTempFile("graph_", ".dot.tmp", new File(GraphViz.TEMP_DIR));
			log.info("write image stream to {}", temp.toString());
			FileWriter fout = new FileWriter(temp);
			fout.write(str);
			fout.close();
		} catch (Exception e) {
			log.error("Error: I/O error while writing the dot source to temp file!", e);
			return null;
		}
		return temp;
	}

	public String start_graph() {
		return GRAPH_START;
	}

	public String end_graph() {
		return GRAPH_END;
	}

	public void readSource(String input) {
		StringBuilder sb = new StringBuilder();

		try {
			FileInputStream fis = new FileInputStream(input);
			DataInputStream dis = new DataInputStream(fis);
			BufferedReader br = new BufferedReader(new InputStreamReader(dis));
			String line;
			while ((line = br.readLine()) != null) {
				sb.append(line);
			}
			dis.close();
		} catch (Exception e) {
			log.error("Error: ", e);
		}

		this.graph = sb;
	}

	public void readString(String dot) {
		this.graph = new StringBuilder(dot);
	}

	public static boolean isValidDotText(String dot) {
		return StringUtils.isNotBlank(dot) && (dot.indexOf(GRAPH_START) > -1);
	}

}
