package example;

// NOTE: bad class naming:
// example.Runner is already defined in this compilation unit
// import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;
import org.openjdk.jmh.runner.options.TimeValue;
import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.results.RunResult;
import org.openjdk.jmh.results.format.ResultFormat;
import org.openjdk.jmh.results.format.ResultFormatFactory;
import org.openjdk.jmh.results.format.ResultFormatType;

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
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import example.utils.Reader;
import example.utils.ReaderBenchmark;

public class Runner {

	private static boolean debug = false;
	private static boolean benchmark = false;

	private static Gson gson = new GsonBuilder().setPrettyPrinting().serializeNulls().create();

	public static void main(String[] args) throws Exception {

		Map<String, String> cli = parseArgs(args);

		String inputFile = "example.bin";
		Long maxRows = 1L;

		if (cli.containsKey("debug")) {
			debug = true;
		}
		if (cli.containsKey("benchmark")) {
			benchmark = true;
		}

		if (debug)
			System.err.println(cli.keySet());

		if (cli.containsKey("help") || !cli.containsKey("inputfile")) {
			System.err.println(String.format("Usage: %s " + "-inputfile <filename>\r\n", "jar"));
			return;
		}
		if (cli.containsKey("inputfile"))
			inputFile = cli.get("inputfile");
		if (benchmark) {
			Options options = new OptionsBuilder()
					// Include the benchmark class(es) you want to run
					.include(ReaderBenchmark.class.getSimpleName())
					// Override the number of warmup iterations (e.g., set to 5)
					.warmupIterations(1)
					// Override the number of measurement iterations (e.g., set to 10)
					.measurementIterations(2)
					// Override the number of forks (e.g., set to 1)
					.warmupTime(TimeValue.milliseconds(100)).measurementTime(TimeValue.milliseconds(100)).forks(1)
					// redirect JMH’s standard textual output
					.output("benchmark.log")
					// Optionally, set the output file
					// NOTE: params must match the properties of the class verbatim
					.param("inputFile", inputFile).build();
			System.err.println(String.format(
					"run with options:" + " fork: %d" + " warmup: %d" + " measurement: %d" + " warmup time: %d"
							+ " measurement time: %d" + " inputFile: %s",
					options.getForkCount().get(), options.getWarmupIterations().get(),
					options.getMeasurementIterations().get(), options.getWarmupTime().get().getTime(),
					options.getMeasurementTime().get().getTime(), options.getParameter("inputFile").get()));

			new org.openjdk.jmh.runner.Runner(options).run();
			// NOTE: need to collect results and print to console

			Collection<RunResult> results = new org.openjdk.jmh.runner.Runner(options).run();

			ResultFormat format = ResultFormatFactory.getInstance(ResultFormatType.TEXT, System.out);
			format.writeOut(results);
		} else {
			new Reader(inputFile).digest();
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
