package example.service;
/**
 * Copyright 2021-2023 Serguei Kouzmine
 */

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.gson.Gson;

import example.utils.ProcessRunner;

@Service
public class ExampleService {

	private Log log = LogFactory.getLog(this.getClass());
	private final String separator = " ";
	private final static String scriptDir = "/var/www/localhost/cgi-bin";
	private boolean debug = false;
	private static Gson gson = new Gson();
	private final String options = "-no-headers";
	protected static String osName = getOSName();
	@Autowired
	ProcessRunner processRunner;

	public ExampleService(ProcessRunner processRunner) {
		this.processRunner = processRunner;
	}

	public void setDebug(boolean data) {
		debug = data;
	}

	public String runCGiBINScript(final String script) {
		return runPerlScript(String.format("%s/%s %s", scriptDir, script, options));
	}

	public String runCGiBINScript(final String script, final String queryString) {
		return runPerlScript(String.format("%s/%s %s", scriptDir, script, options),
				queryString);
	}

	public String runCGiBINScript(final String script, String[] commandlineArgs) {
		// ignore command line args for now
		return runPerlScript(String.format("%s/%s %s %s", scriptDir, script,
				options, String.join(separator, Arrays.asList(commandlineArgs))));
	}

	public String runCGiBINScript(final String script, final String queryString,
			final String payload) {
		return runPerlScript(String.format("%s/%s %s", scriptDir, script, options),
				queryString, payload);
	}

	public String runPerlScript(String processName) {
		return runPerlScript(processName, null, null);
	}

	public String runPerlScript(String processName, String queryString) {
		return runPerlScript(processName, queryString, null);
	}

	public String runPerlScript(String perlScriptWithArgs, String queryString,
			String payload) {
		log.info("Running the perl script with arguments: " + perlScriptWithArgs);

		if (perlScriptWithArgs.isEmpty()) {
			return null;
		}
		String command = String.format(
				(osName.equals("windows")) ? "perl.exe %s" : "/usr/bin/perl %s",
				perlScriptWithArgs.trim());
		return runProcess(command, queryString, payload);

	}

	public String runProcess(String command) {
		return runProcess(command, null, null);
	}

	public String runProcess(String command, String queryString) {
		return runProcess(command, queryString, null);
	}

	public String runProcess(String command, String queryString, String payload) {

		processRunner.runProcess(command, queryString, payload);

		if (processRunner.isStatus()) {
			return processRunner.getProcessOutput();
		} else {
			Map<String, Object> failureResult = new HashMap<>();
			failureResult.put("status", processRunner.isStatus());
			failureResult.put("stdout",
					processRunner.getProcessOutput().replaceAll("\"", "'"));
			failureResult.put("stderr",
					processRunner.getProcessError().replaceAll("\"", "'"));
			failureResult.put("exitcode", processRunner.getExitCode());
			log.info("returning error from command: " + command);
			return gson.toJson(failureResult);
		}

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
