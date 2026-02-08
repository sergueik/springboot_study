package example;

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


import org.apache.commons.lang3.StringUtils;
import example.GraphvizGraalEngine;
import example.Config;

public class GraphViz {

    private static final Logger log = LoggerFactory.getLogger(GraphViz.class);
    private static volatile boolean ready = false;
    private static GraphvizGraalEngine graalEngine;

    public static void init() {
        try {
            String engine = Config.get("graphviz.engine", "jdk");
            log.info("Using Graphviz engine: {}", engine);

            switch (engine.toLowerCase()) {
                case "graal":
                    graalEngine = new GraphvizGraalEngine(); // initialize GraalJS engine
                    break;
                case "jdk":
                default:
                    log.warn("Only Graal engine is supported in this minimal setup. Falling back is not implemented.");
                    break;
            }
            log.info("Graphviz engine, {} created", graalEngine.hashCode());

            // optional warmup
            String warmupDot = "graph { a -- b }";
            byte[] warmup = graalEngine.renderPng(warmupDot);
            log.info("Graphviz engine warmup complete, {} bytes generated", warmup.length);

            ready = true;
        } catch (Exception e) {
            log.error("Graphviz warmup failed", e);
            ready = false;
        }
    }

    public static boolean isReady() {
        return ready;
    }

	private File writeDotSourceToFile(String str) throws IOException {
		File tempFile;
		try {
			tempFile = File.createTempFile("graph_", ".dot.tmp", new File(GraphViz.TEMP_DIR));
			log.info("write image stream to {}", tempFile.toString());
			FileWriter fileWriter = new FileWriter(tempFile);
			fileWriter.write(str);
			fileWriter.close();
		} catch (Exception e) {
			log.error("Error: I/O error while writing the dot source to temp file!", e);
			return null;
		}
		return tempFile;
	}

    private StringBuilder graph = new StringBuilder();
    private static final String TEMP_DIR = "/tmp"; // or "c:/temp" for Windows
    public static final String GRAPH_START = "graph {";
    public static final String GRAPH_END = "}";

    public GraphViz() {
    }

    public void add(String line) {
        graph.append(line);
    }

    public void addln(String line) {
        graph.append(line).append('\n');
    }

    public void addln() {
        graph.append('\n');
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
        if (img == null) return -1;
        try (FileOutputStream fos = new FileOutputStream(new File(filePath))) {
            fos.write(img);
        } catch (IOException e) {
            log.error("Error writing image to file", e);
            return -1;
        }
        return 1;
    }

    public String startGraph() {
        return GRAPH_START;
    }

    public String endGraph() {
        return GRAPH_END;
    }

    public static void closeEngine() {
        if (graalEngine != null) {
            graalEngine.close();
        }
    }
	public static boolean isValidDotText(String dot) {
		return StringUtils.isNotBlank(dot) && (dot.indexOf(GRAPH_START) > -1);
	}

}


