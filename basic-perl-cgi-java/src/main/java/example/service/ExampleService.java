package example.service;
/**
 * Copyright 2021 Serguei Kouzmine
 */

import java.io.BufferedReader;
import java.io.InputStreamReader;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.stereotype.Service;

@Service
public class ExampleService {

	private Log log = LogFactory.getLog(this.getClass());

	private final static String scriptDir = "/var/www/localhost/cgi-bin";
	private final String options = "-no-headers";
	protected static String osName = getOSName();

	public String runScript(final String script) {
		return runProcess(String.format("%s/%s %s", scriptDir, script, options));
	}

	public String runScript(final String script, String[] commandlineArgs) {
		// ignore command line args for now
		return runProcess(String.format("%s/%s %s", scriptDir, script, options));
	}

	// https://www.javaworld.com/article/2071275/core-java/when-runtime-exec---won-t.html?page=2
	public String runProcess(String processName) {
		BufferedReader stderrBufferedReader;
		BufferedReader stdoutBufferedReader;
		StringBuffer processOutput = new StringBuffer();
		StringBuffer processError = new StringBuffer();
		log.info("Running the process: " + processName);

		if (processName.isEmpty()) {
			return null;
		}
		String command = String.format(
				(osName.equals("windows")) ? "perl.exe %s" : "/usr/bin/perl %s",
				processName.trim());
		try {
			Runtime runtime = Runtime.getRuntime();
			Process process = runtime.exec(command);
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