package example.controller;
/**
 * Copyright 2021,2023,2024 Serguei Kouzmine
 */

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

import java.nio.file.Path;
import java.nio.file.Paths;

import java.util.Optional;

import org.springframework.core.io.ClassPathResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseCookie;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import java.util.ArrayList;
// import org.apache.commons.codec.binary.Base64;
import java.util.Base64;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@RestController
@RequestMapping("/basic")
public class Controller {

	private static String osName = getOSName();

	private static final Logger logger = LoggerFactory
			.getLogger(Controller.class);

	private static final StringBuilder data = new StringBuilder();
	private final static String default_value = "default_value";
	private final static String default_name = "default_name";

	@GetMapping(produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<String> hello() {
		return ResponseEntity.ok().build();
	}

	@GetMapping("/init")
	public ResponseEntity<String> setCookie() {

		ResponseCookie resCookie = ResponseCookie
				.from(default_name, "c2FtLnNtaXRoQGV4YW1wbGUuY29t").httpOnly(true)
				.secure(true).path("/").maxAge(1 * 24 * 60 * 60).domain("localhost")
				.build();
		return ResponseEntity.ok()
				.header(HttpHeaders.SET_COOKIE, resCookie.toString()).build();

	}

	// see also
	// https://github.com/thombergs/code-examples/blob/master/spring-boot/cookie-demo/src/main/java/io/reflectoring/cookie/controllers/SpringCookieController.java
	// https://www.baeldung.com/cookies-java
	@GetMapping("/clear")
	public ResponseEntity<String> deleteCookie() {

		// create a cookie
		ResponseCookie resCookie = ResponseCookie.from(default_name, null).build();
		return ResponseEntity.ok()
				.header(HttpHeaders.SET_COOKIE, resCookie.toString()).build();
	}

	@RequestMapping(value = "/simpleupload", method = RequestMethod.POST, consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
	public ResponseEntity<String> uploadFile(
			@RequestParam("file") MultipartFile file) {

		if (file.isEmpty()) {
			logger.info("file argument cannot be empty");
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
		try {
			InputStream in = file.getInputStream();
			data.setLength(0);
			int ch = 0;
			while ((ch = in.read()) != -1) {
				data.append(new Character((char) ch).toString());
			}
			logger.info("data size: " + data.length()  /* + "\n"
					+ "raw data(base64 encoded):" + "\n" + data.toString() */);
		} catch (IOException e) {
			logger.info("Exception (caught):" + e.toString());
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
		return new ResponseEntity<String>(HttpStatus.OK);
	}

	@RequestMapping(value = "/upload", method = RequestMethod.POST, consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
	public ResponseEntity<String> upload(
			@CookieValue(name = default_name, defaultValue = default_value) String cookie,
			@RequestParam("operation") String operation,
			@RequestParam("param") String param,
			@RequestParam("servername") String servername,
			@RequestParam("encode") Optional<Boolean> encode,
			@RequestParam("file") MultipartFile file) {
		if (param.isEmpty()) {
			logger.info("param can not be empty");
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
		if (servername.isEmpty()) {
			logger.info("servername can not be empty");
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
		Path basePath = Paths.get(String.format(
				"%s%ssrc%smain%sresources%sdata%s%s",
				(osName.equals("windows")
						? System.getProperty("user.dir").replaceAll("/", "\\")
						: System.getProperty("user.dir")),
				File.separator, File.separator, File.separator, File.separator,
				File.separator, servername));

		mkdirs(basePath.toFile());
		if (!operation.equals("send")) {
			logger.info("unsupported operation: " + operation);
			return ResponseEntity.status(HttpStatus.METHOD_NOT_ALLOWED).body(null);
		} else {
			if (file.isEmpty()) {
				return ResponseEntity.status(HttpStatus.NOT_FOUND).body(null);
			}
			try {

				String currDirPath = new File(".").getAbsolutePath();
				String uploadFilePath = currDirPath.substring(0,
						currDirPath.length() - 1) + file.getOriginalFilename();
				logger.info(
						"Processing " + file.getOriginalFilename() + " " + uploadFilePath);
				data.setLength(0);
				InputStream in = file.getInputStream();
				FileOutputStream f = new FileOutputStream(uploadFilePath);
				int ch = 0;
				while ((ch = in.read()) != -1) {
					f.write(ch);
					data.append(new Character((char) ch).toString());
				}
				f.flush();
				f.close();

				// origin:
				// http://www.java2s.com/example/java-utility-method/base64-encode/base64encode-byte-data-281b5.html
				// NOTE: still incorrect
				if (encode.isPresent() && encode.get()) {
					byte[] binaryData = data.toString().getBytes();
					Base64.Encoder encoder = Base64.getEncoder();
					String base64EncodedData = encoder.encodeToString(binaryData);
					logger
							.info(String.format(
									"data size: %d/%d" + "\n" + "raw data(base64 encoded):" + "\n"
											+ "%s",
									data.length(), binaryData.length, base64EncodedData));
				} else
					logger.info("data size: " + data.length() + "\n"
							+ "raw data(base64 encoded):" + "\n" + data.toString());

			} catch (IOException e) {
				logger.info("Exception (caught):" + e.toString());
				return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
			}
			return ResponseEntity.status(HttpStatus.OK).body(data.toString());
		}
	}

	//
	@RequestMapping(value = "/classpath", produces = MediaType.APPLICATION_OCTET_STREAM_VALUE, method = RequestMethod.GET)
	public ResponseEntity<String> getClassPathResourcePath(
			@RequestParam(value = "file", required = false) String file) {
		String result = null;

		try {
			ClassPathResource resource = new ClassPathResource(
					(file == null) ? "" : file);
			result = resource.getFile().getAbsolutePath();
			logger.info("classpath: " + result);
			return ResponseEntity.status(HttpStatus.OK).body(result);
		} catch (IOException e) {
			logger.info("Exception (caught):" + e.toString());
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}

	}

	// origin: https://qna.habr.com/q/1258736
	// see also:
	@RequestMapping(value = "/partialupload", produces = MediaType.APPLICATION_OCTET_STREAM_VALUE, method = RequestMethod.POST)
	public ResponseEntity<byte[]> partialUpload(
			@RequestHeader(value = "Range", required = false, defaultValue = "") String range)
			throws IOException {
		// System.err.println("Range is: " + range);
		ClassPathResource file = new ClassPathResource("test.txt");
		InputStream is = file.getInputStream();
		HttpHeaders headers = new HttpHeaders();
		int fileSize = is.available();
		byte[] data;
		String[] rangeArray = range.split("=");
		int byteStart = 0;
		int byteEnd = fileSize - 1;

		if (rangeArray.length > 1) {
			String[] byteRange = rangeArray[1].split("-");
			byteStart = Integer.parseInt(byteRange[0]);
			if (byteRange.length > 1) {
				byteEnd = Integer.parseInt(byteRange[1]);
			}
		}

		int contentLength = byteEnd - byteStart + 1;
		data = new byte[contentLength];
		is.skip(byteStart);
		is.read(data, 0, contentLength);
		is.close();

		headers.add("Content-Type", "text/plain");
		headers.add("Content-Length", String.valueOf(contentLength));
		headers.add("Accept-Ranges", "bytes");
		headers.add("Content-Range",
				"bytes " + byteStart + "-" + byteEnd + "/" + fileSize);

		return new ResponseEntity(data, headers, HttpStatus.PARTIAL_CONTENT);
	}

	public static void mkdirs(File dir) {
		File parent = dir.getAbsoluteFile();
		List<File> mkdir = new ArrayList<File>();
		for (; !parent.exists()
				|| !parent.isDirectory(); parent = parent.getParentFile()) {
			mkdir.add(parent);
		}
		for (int i = mkdir.size(); --i >= 0;) {
			File d = mkdir.get(i);
			d.mkdir();
			d.setReadable(true, false);
			d.setWritable(true, false);
		}
	}

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
