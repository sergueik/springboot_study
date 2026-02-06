package example;

import static guru.nidi.graphviz.attribute.Records.*;
import static guru.nidi.graphviz.model.Compass.*;

import java.io.File;
import java.io.InputStream;
import java.nio.channels.FileChannel;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
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
		String inputFile = "color.dot";
		String outputFile = "ex4-1.png";
		File properties = new File(inputFile);

		if (properties.exists()) {
			try {
				Graphviz.useEngine(new GraphvizJdkEngine());
				FileInputStream dot = new FileInputStream(properties);
				MutableGraph g = new Parser().read(dot);
				Graphviz.fromGraph(g).width(700).render(Format.PNG).toFile(new File(outputFile));
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

}
