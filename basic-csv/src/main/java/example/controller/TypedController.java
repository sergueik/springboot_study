package example.controller;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.FormHttpMessageConverter;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;
import org.apache.commons.csv.CSVRecord;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;

@RestController
@RequestMapping("/")
public class TypedController {

	private static Gson gson = new GsonBuilder().create();
	private boolean debug = false;
	public static final String[] HEADERS = { "author", "title", "year", "isbn" };

	enum BookHeaders {
		author, title, year, isbn
	}

	private final Logger logger = LoggerFactory.getLogger(TypedController.class);
	private static final StringBuilder data = new StringBuilder();


	public static class Data {

		private boolean status;
		private String author;
		private String title;

		public boolean isStatus() {
			return status;
		}

		public void setStatus(boolean status) {
			this.status = status;
		}

		public String getAuthor() {
			return author;
		}

		public void setAuthor(String data) {
			author = data;
		}

		public String getTitle() {
			return title;
		}

		public void setTitle(String data) {
			title = data;
		}

		public Data(String author, String title) {
			this.title = title;
			this.author = author;
		}

		public Data() {
		}

		@Override
		public String toString() {

			return "Data {" + "title=" + this.title + " " + "author=" + this.author
					+ '}';
		}
	}

	private List<Data> processData(StringReader in) throws IOException {

		List<Data> result = new ArrayList<>();
		try {
			CSVFormat csvFormat = CSVFormat.DEFAULT.builder().setHeader(HEADERS)
					.setSkipHeaderRecord(true).build();

			Iterable<CSVRecord> records;
			records = csvFormat.parse(in);
			logger.info("Before Reading: ");
			for (CSVRecord record : records) {
				logger.info("Reading: ");
				String author = record.get("author");
				String title = record.get("title");
				Data data = new Data(author, title);
				logger.info("Read: " + data.toString());
				result.add(data);
			}
		} catch (IOException e) {
			logger.info("Exception: " + e.toString());
			throw (e);
		}
		return result;

	}

	@PostMapping(value = "/encodeddata", produces = {
			MediaType.APPLICATION_JSON_VALUE }, consumes = {
					MediaType.APPLICATION_FORM_URLENCODED_VALUE })
	public ResponseEntity<String> encodeddata(@RequestBody String body) {

		List<Data> result = new ArrayList<>();
		try {
			String decodedBody = URLDecoder.decode(body, "UTF-8");
			logger.info(String.format("Decoded Body: \"%s\"", decodedBody));
			StringReader in = new StringReader(body);
			result = processData(in);
		} catch (UnsupportedEncodingException e) {
			logger.info("Exception: " + e.toString());
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);

		} catch (IOException e) {
			logger.info("Exception: " + e.toString());
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
		return ResponseEntity.status(HttpStatus.OK).body(gson.toJson(result));
	}

	@PostMapping(value = "/data", produces = {
			MediaType.APPLICATION_JSON_VALUE }, consumes = MediaType.APPLICATION_OCTET_STREAM_VALUE)
	public ResponseEntity<String> data(@RequestBody String body) {

		List<Data> result;
		StringReader in = new StringReader(body);
		logger.info(String.format("Body: \"%s\"", body));
		try {
			result = processData(in);
		} catch (UnsupportedEncodingException e) {
			logger.info("Exception: " + e.toString());
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);

		} catch (IOException e) {
			logger.info("Exception: " + e.toString());
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
		return ResponseEntity.status(HttpStatus.OK).body(gson.toJson(result));
	}

	// origin:
	@RequestMapping(value = "/upload", method = RequestMethod.POST)
	public ResponseEntity<String> upload(
			@RequestParam("operation") String operation,
			@RequestParam("param") String param,
			@RequestParam("file") MultipartFile file) {
		if (param.isEmpty())
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		if (!operation.equals("send")) {
			logger.error("invalid operation: " + operation);
			return ResponseEntity.status(HttpStatus.METHOD_NOT_ALLOWED).body(null);
		} else {
			if (file.isEmpty()) {
				return ResponseEntity.status(HttpStatus.NOT_FOUND).body(null);
			}
			try {
				logger.info("Processing " + file.getOriginalFilename());
				data.setLength(0);
				InputStream in = file.getInputStream();
				String currDirPath = new File(".").getAbsolutePath();
				FileOutputStream f = new FileOutputStream(
						currDirPath.substring(0, currDirPath.length() - 1)
								+ file.getOriginalFilename());
				int ch = 0;
				while ((ch = in.read()) != -1) {
					f.write(ch);
					data.append(new Character((char) ch).toString());
				}
				f.flush();
				f.close();
				System.err.print(data.toString());
			} catch (IOException e) {
				logger.error("Exception (caught):" + e.toString());
				return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
			}
			return ResponseEntity.status(HttpStatus.OK).body(data.toString());
		}

	}
}
