package example;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.InputStreamReader;

import org.apache.commons.lang3.StringUtils;

public class GraphViz {

	private static final Logger log = LoggerFactory.getLogger(GraphViz.class);

	private static String TEMP_DIR = "/tmp"; // Linux
	// private static String TEMP_DIR = "c:/temp"; // Windows

	// private static String DOT = "/usr/local/bin/dot"; // MAC
	private static String DOT = "/usr/bin/dot"; // Linux
	// private static String DOT = "c:/Program Files/Graphviz/bin/dot.exe"; //
	// Windows

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
		} catch (java.io.IOException ioe) {
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

	private byte[] get_img_stream(File dot, String type) {
		File img;
		byte[] img_stream = null;
		log.info("get image stream from {}", dot.toString());
		try {
			img = File.createTempFile("graph_", "." + type, new File(GraphViz.TEMP_DIR));
			Runtime rt = Runtime.getRuntime();

			// patch by Mike Chenault
			String[] args = { DOT, "-T" + type, dot.getAbsolutePath(), "-o", img.getAbsolutePath() };
			Process p = rt.exec(args);

			p.waitFor();

			FileInputStream in = new FileInputStream(img.getAbsolutePath());
			img_stream = new byte[in.available()];
			in.read(img_stream);
			// Close it if we need to
			if (in != null)
				in.close();

			if (img.delete() == false)
				log.warn("Warning: " + img.getAbsolutePath() + " could not be deleted!");
		} catch (java.io.IOException ioe) {
			log.warn("Error:    in I/O processing of tempfile in dir " + GraphViz.TEMP_DIR + "\n");
			log.warn("       or in calling external command");
			log.error("stacktrace", ioe);
		} catch (java.lang.InterruptedException ie) {
			log.error("Error: the execution of the external program was interrupted", ie);
		}

		return img_stream;
	}

	private File writeDotSourceToFile(String str) throws java.io.IOException {
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
