package info.fetter.rrdclient;

/*
 * Copyright 2013 Didier Fetter
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.WritableByteChannel;
import java.util.ArrayList;
import java.util.Date;

import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;
import org.apache.log4j.Logger;

/**
 * 
 * Wrapper for the RRD fetch command.
 * 
 * @author Didier Fetter
 *
 */
public class FetchCommand extends RRDCommand {
	@SuppressWarnings("unused")
	private static Logger logger = Logger.getLogger(FetchCommand.class);
	String fileName, consolidationFunction;
	String[] args;
	DataTable<Double> table = new DataTable<Double>();
	boolean isOutputParsed = false;

	/**
	 * Create a wrapper object for the RRD fetch command.
	 * 
	 * @param fileName name of the rrd file to query
	 * @param consolidationFunction AVERAGE, LAST, MAX
	 * @param args additional arguments : start, end, resolution...
	 */
	public FetchCommand(String fileName, String consolidationFunction, String... args) {
		try {
			this.fileName = fileName;
			this.consolidationFunction = consolidationFunction;
			this.args = args;
			CommandLineParser parser = new PosixParser();
			Options options = new Options();
			options.addOption("r", "resolution", true, "resolution (default is the highest resolution)");
			options.addOption("s", "start", true, "start (default end-1day)");
			options.addOption("e", "end", true, "end (default now)");
			parser.parse(options, args);
		} catch (ParseException e) {
			throw new IllegalArgumentException(e);
		}
	}

	@Override
	public void execute(OutputStream out) {
		String command = "fetch " + fileName + " " + consolidationFunction;
		for(String arg : args ) {
			command += " " + arg;
		}
		try {
			ByteBuffer response = sendCommandToServer(command);
			WritableByteChannel channel = Channels.newChannel(out);
			channel.write(response);
			isOutputParsed = false;
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public void execute() {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		execute(out);
		try {
			parseOutput(out.toByteArray());
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	protected void parseOutput(byte[] output) throws IOException {
		if(isOutputParsed) return;

		ByteArrayInputStream in = new ByteArrayInputStream(output);
		BufferedReader reader = new BufferedReader(new InputStreamReader(in));

		parseFirstLine(reader.readLine());
		reader.readLine();

		String line;		
		while((line = reader.readLine()) != null) {
			//logger.trace(line);
			if(line.startsWith("OK")) {
				parseStatusLine(line);
			} else {
				parseDataLine(line);
			}
		}

		isOutputParsed = true;
	}

	private void parseDataLine(String line) {
		String[] explodedLine = line.split(":");
		String dateString, dataString;
		try {
			dateString = explodedLine[0];
			dataString = explodedLine[1].trim();

			Date date = new Date(Long.parseLong(dateString)*1000);
			String[] dataArray = dataString.split(" ");
			ArrayList<Double> data = new ArrayList<Double>(dataArray.length);
			for(String value : dataArray) {
				try {
					data.add(Double.parseDouble(value));
				} catch(NumberFormatException e) {
					data.add(Double.NaN);
				}
			}
			table.addRow(date, data);
		} catch(Exception e) {
			//logger.debug("Error parsing line : " + line);
			throw new RuntimeException("Error parsing line : " + line, e);
		}
	}

	private void parseFirstLine(String line) {
		line = line.trim();
		verifyIfError(line);
		String[] lineExploded = line.split(" +");
		for(String columnName : lineExploded) {
			table.addColumn(columnName);
		}
	}

	/**
	 * 
	 * Get the data returned by the fetch command in a tabular form.
	 * 
	 * @return the data table returned by the fetch command
	 */
	public DataTable<Double> getDataTable() {
		return table;
	}



}
