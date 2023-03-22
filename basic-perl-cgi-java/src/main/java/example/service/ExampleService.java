package example.service;
/**
 * Copyright 2021-2023 Serguei Kouzmine
 */

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.OutputStreamWriter;
import java.util.Arrays;
import java.io.InputStreamReader;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.stereotype.Service;

@Service
public class ExampleService {

	private Log log = LogFactory.getLog(this.getClass());
	private final String separator = " ";
	private final static String scriptDir = "/var/www/localhost/cgi-bin";
	private final String options = "-no-headers";
	protected static String osName = getOSName();

	public String runCGiBINScript(final String script) {
		return runPerlScript(String.format("%s/%s %s", scriptDir, script, options));
	}

	public String runCGiBINScript(final String script, String[] commandlineArgs) {
		// ignore command line args for now
		return runPerlScript(String.format("%s/%s %s %s", scriptDir, script,
				options, String.join(" ", Arrays.asList(commandlineArgs))));
	}

	public String runCGiBINScript(final String script, String payload) {
		return runPerlScript(String.format("%s/%s %s", scriptDir, script, options),
				payload);
	}

	public String runPerlScript(String processName) {
		return runPerlScript(processName, null);
	}

	public String runPerlScript(String perlScriptWithArgs, String payload) {
		log.info("Running the perl script with arguments: " + perlScriptWithArgs);

		if (perlScriptWithArgs.isEmpty()) {
			return null;
		}
		String command = String.format(
				(osName.equals("windows")) ? "perl.exe %s" : "/usr/bin/perl %s",
				perlScriptWithArgs.trim());
		return runProcess(command, payload);

	}

	public String runProcess(String command) {
		return runProcess(command, null);
	}

	// https://www.javaworld.com/article/2071275/core-java/when-runtime-exec---won-t.html?page=2
	public String runProcess(String command, String payload) {
		BufferedReader stderrBufferedReader;
		BufferedReader stdoutBufferedReader;
		StringBuffer processOutput = new StringBuffer();
		StringBuffer processError = new StringBuffer();
		try {
			Runtime runtime = Runtime.getRuntime();

			Process process = null;

			if (payload != null) {
				// see also:
				// execute the specified string command in a separate process
				// with the specified environment
				// http://docs.oracle.com/javase/6/docs/api/java/lang/Runtime.html#exec%28java.lang.String,%20java.lang.String%5B%5D%29
				String[] envp = { String.format("CONTENT_LENGTH=%d", payload.length()),
						"REQUEST_METHOD=POST" };
				log.info("Running with environment: " + Arrays.asList(envp));
				process = runtime.exec(command, envp);
				BufferedWriter bufferedWriter = new BufferedWriter(
						new OutputStreamWriter(process.getOutputStream()));
				log.info("Passing the payload: " + payload);
				// Passing the payload: %7B%22foo%22%3A+%22bar%22%7D=
				bufferedWriter.write(payload);
				bufferedWriter.newLine();
				bufferedWriter.flush();
				bufferedWriter.close();
			} else {
				process = runtime.exec(command);
			}
			// process.redirectErrorStream( true);

			stdoutBufferedReader = new BufferedReader(
					new InputStreamReader(process.getInputStream()));

			stderrBufferedReader = new BufferedReader(
					new InputStreamReader(process.getErrorStream()));
			String line = null;

			while ((line = stdoutBufferedReader.readLine()) != null) {
				processOutput.append(line);
			}

			while ((line = stderrBufferedReader.readLine()) != null) {
				processError.append(line);
			}
			int exitCode = process.waitFor();
			// ignore exit code 128
			if (exitCode != 0 && (exitCode ^ 128) != 0) {
				log.info("Process exit code: " + exitCode);
				if (processOutput.length() > 0) {
					log.info("<OUTPUT>" + processOutput + "</OUTPUT>");
				}
				if (processError.length() > 0) {
					log.info("<ERROR>" + processError + "</ERROR>");
				}
			}
		} catch (Exception e) {
			log.info("Exception (ignored): " + e.getMessage());
		}
		return processOutput.toString();
	}

	// Utilities
	public static String getOSName() {
		if (osName == null) {
			osName = System.getProperty("os.name").toLowerCase();
			if (osName.startsWith("windows")) {
				osName = "windows";
			}
		}
		return osName;
	}

}
