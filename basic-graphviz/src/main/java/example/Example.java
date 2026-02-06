package example;

import static guru.nidi.graphviz.attribute.Records.*;
import static guru.nidi.graphviz.model.Compass.*;

import java.io.File;
import java.io.InputStream;
import java.nio.channels.FileChannel;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.HashMap;
import java.util.Map;
import java.io.File;
import java.io.FileInputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;
import guru.nidi.graphviz.engine.GraphvizCmdLineEngine;
import guru.nidi.graphviz.engine.GraphvizJdkEngine;
import guru.nidi.graphviz.model.MutableGraph;
import guru.nidi.graphviz.parse.Parser;

public class Example {
	private final static Logger logger = LoggerFactory.getLogger(Example.class);
	private static boolean debug = false;

	public static void main(String[] args) throws Exception {
		String inputFile = null;
		String outputFile = null;

		Map<String, String> cli = parseArgs(args);

		if (cli.containsKey("debug")) {
			debug = true;
		}
		if (debug)
			System.err.println(cli.keySet());

		if (cli.containsKey("help") || !cli.containsKey("outputfile") || !cli.containsKey("inputfile")) {
			System.err.println(String.format("Usage: %s " + "-outputFile <filename> -inputfile <filename>\r\n", "jar"));
			return;
		}
		if (cli.containsKey("inputfile"))
			inputFile = cli.get("inputfile");
		if (cli.containsKey("outputfile"))
			outputFile = cli.get("outputfile");
		logger.info("converting {} {}", inputFile, outputFile);
		File properties = new File(inputFile);

		if (!properties.exists())
			return;
		try {
			Graphviz.useEngine(new GraphvizJdkEngine());
			FileInputStream dot = new FileInputStream(properties);
			MutableGraph g = new Parser().read(dot);
			Graphviz.fromGraph(g).width(700).render(Format.PNG).toFile(new File(outputFile));
		} catch (Exception e) {
			e.printStackTrace();
		}

	}

	// Extremely simple CLI parser: -key value
	private static Map<String, String> parseArgs(String[] args) {
		Map<String, String> map = new HashMap<>();
		for (int i = 0; i < args.length - 1; i++) {
			if (args[i].startsWith("-")) {
				map.put(args[i].substring(1), args[i + 1]);
				i++;
			}
		}
		return map;
	}
}
