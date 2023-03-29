package example.utils;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.Arrays;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.stereotype.Service;

public class ProcessRunner {

	private Log log = LogFactory.getLog(this.getClass());
	private BufferedReader stderrBufferedReader;
	private BufferedReader stdoutBufferedReader;
	private BufferedWriter bufferedWriter;
	private StringBuffer processOutputBuffer = new StringBuffer();
	private StringBuffer processErrorBuffer = new StringBuffer();
	private boolean status = false;
	private Process process = null;
	private Runtime runtime;
	private String line = null;

	public ProcessRunner() {
		processOutputBuffer = new StringBuffer();
		processErrorBuffer = new StringBuffer();
	}

	private int exitCode;
	// TODO: make regular class with properties
	private String processOutput = "";
	private String processError = "";

	public boolean isStatus() {
		return status;
	}

	public int getExitCode() {
		return exitCode;
	}

	public String getProcessOutput() {
		return processOutput;
	}

	public String getProcessError() {
		return processError;
	}

	public void runProcess(String command) {
		runProcess(command, null);
	}

	// https://www.javaworld.com/article/2071275/core-java/when-runtime-exec---won-t.html?page=2
	// see also:
	// execute the specified string command in a separate process
	// with the specified environment
	// http://docs.oracle.com/javase/6/docs/api/java/lang/Runtime.html#exec%28java.lang.String,%20java.lang.String%5B%5D%29
	public void runProcess(String command, String payload) {
		processOutputBuffer.setLength(0);
		processErrorBuffer.setLength(0);
		try {
			runtime = Runtime.getRuntime();

			if (payload != null) {
				String[] envp = { String.format("CONTENT_LENGTH=%d", payload.length()),
						"REQUEST_METHOD=POST" };
				log.info("Running with environment: " + Arrays.asList(envp));
				process = runtime.exec(command, envp);
				bufferedWriter = new BufferedWriter(
						new OutputStreamWriter(process.getOutputStream()));
				log.info("Passing the payload: " + payload);
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

			while ((line = stdoutBufferedReader.readLine()) != null) {
				processOutputBuffer.append(line);
			}

			while ((line = stderrBufferedReader.readLine()) != null) {
				processErrorBuffer.append(line);
			}
			exitCode = process.waitFor();
			status = true;
			// ignore Windows-specific exit code 128
			if (exitCode != 0 && (exitCode ^ 128) != 0) {
				status = false;
				log.info("Process exit code: " + exitCode);
				if (processOutputBuffer.length() > 0) {
					log.info("<OUTPUT>" + processOutputBuffer + "</OUTPUT>");
				}
				if (processErrorBuffer.length() > 0) {
					log.info("<ERROR>" + processErrorBuffer + "</ERROR>");
					processError = processErrorBuffer.toString();
				}
			}
		} catch (Exception e) {
			log.info("Exception (ignored): " + e.getMessage());
		}
		processOutput = processOutputBuffer.toString();
	}
}
