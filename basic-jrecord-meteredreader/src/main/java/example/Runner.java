package example;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import example.utils.Reader;


public class Runner {

	private final static Logger logger = LoggerFactory.getLogger(Runner.class);
	private static boolean debug = false;
	private static boolean benchmark = false;

	private static Gson gson = new GsonBuilder().setPrettyPrinting().serializeNulls().create();

	public static void main(String[] args) throws Exception {

		Map<String, String> cli = parseArgs(args);

		String copybookFile = "example.cbl";
		String inputFile = "example.bin";
		Long maxRows = 1L;
		String page = "cp037";

		if (cli.containsKey("debug")) {
			debug = true;
		}
		if (cli.containsKey("benchmark")) {
			benchmark = true;
		}

		if (debug)
			System.err.println(cli.keySet());

		if (cli.containsKey("help") || !cli.containsKey("copybookfile") || !cli.containsKey("inputfile")) {
			System.err.println(String.format(
					"Usage: %s " + "-copybookfile <filename> -outputfile <filename> page <ACP> -maxrows <number>\r\n",
					"jar"));
			return;
		}

		if (cli.containsKey("inputFile"))
			inputFile = cli.get("inputFile");
		if (cli.containsKey("copybookfile"))
			copybookFile = cli.get("copybookfile");

		if (cli.containsKey("page"))
			page = cli.get("page");
		if (cli.containsKey("maxrows"))
			maxRows = Long.parseLong(cli.get("maxrows"));
		if (benchmark) {
			// filter command line arguments separate Reader's app flags from JMH's:
			// JMH is said to ignore unknown arguments but 
			// it will actually attempt to parse them and fail
			List<String> jmhArgs = new ArrayList<>();
			for (int i = 0; i < args.length; i++) {
				String arg = args[i];
				if (arg.matches("(?:-p|-bm|-f|-wi)")) {
					jmhArgs.add(arg);
					if (i + 1 < args.length)
						jmhArgs.add(args[++i]);
				}
			}
			jmhArgs.addAll(Arrays.asList("-r 1 -wi 1 -f 1".split("\\s+")));
			if (debug)
				System.err.printf("Run jmh benchmarks with %s", String.join(" ", jmhArgs));
			org.openjdk.jmh.Main.main(jmhArgs.toArray(new String[0]));
		} else
			new Reader(copybookFile, inputFile, page, maxRows).parseRecords();
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
