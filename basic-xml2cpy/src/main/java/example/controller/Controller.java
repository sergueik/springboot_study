package example.controller;
/*
 * Copyright 2026 Serguei Kouzmine
 */

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.io.Reader;
import java.util.Optional;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.springframework.core.io.ClassPathResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseCookie;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.w3c.dom.Document;

import net.sf.cb2xml.Cb2Xml;
import net.sf.cb2xml.Cb2Xml3;
import net.sf.cb2xml.ICb2XmlBuilder;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.sablecc.parser.ParserException;
import java.util.ArrayList;
// import org.apache.commons.codec.binary.Base64;
import java.util.Base64;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

@RestController
@RequestMapping("/copybook")
public class Controller {

	private static String osName = getOSName();
	private String xml = null;
	private static final Logger logger = LoggerFactory.getLogger(Controller.class);

	private static final StringBuilder data = new StringBuilder();

	@PostMapping(value = "/to", produces = MediaType.APPLICATION_XML_VALUE, consumes = { MediaType.TEXT_PLAIN_VALUE,
			MediaType.MULTIPART_FORM_DATA_VALUE })
	public ResponseEntity<String> toXml(@RequestParam(required = false) MultipartFile file,
			@RequestBody(required = false) String body,
			@RequestParam(name = "indent", required = false) Optional<Boolean> indent,
			@RequestParam(name = "format", required = false) Optional<String> format) throws Exception {

		// alternatively, to shorten the checks and getter
		// boolean doIndent = indent.orElse(false);
		// String lineFormat = format.orElse("FREE");

		logger.info("reading: {}", body);
		String content = extractContent(file, body);
		if (content == null) {
			return ResponseEntity.badRequest().body("No input provided");
		}
		// write temp file to meeet the cbl2xml builder signature
		File tempFile = File.createTempFile("cpy_", ".tmp");
		Files.writeString(Paths.get(tempFile.getCanonicalPath()), content);
		logger.info("saved to: {}", tempFile.getCanonicalPath());

		// https://github.com/bmTas/cb2xml/blob/master/src/main/java/net/sf/cb2xml/Cb2Xml3.java#L90
		// NOTE: the format query param has no effect in this version of cbl2xml
		ICb2XmlBuilder cb2XmlBuilder = Cb2Xml3.newBuilder(tempFile)
				.setCobolLineFormat(format.isEmpty() ? Cb2xmlConstants.FREE_FORMAT
						: ("FIXED".equalsIgnoreCase(format.get())) ? Cb2xmlConstants.FREE_FORMAT : Cb2xmlConstants.FREE_FORMAT)
				.setIndent(indent.isPresent());

		// https://github.com/bmTas/cb2xml/blob/master/src/main/java/net/sf/cb2xml/Cb2Xml.java#L184
		try {
			xml = cb2XmlBuilder.asXmlString();
			// remove the unnecessary XML declaration produced by cb2XmlBuilder
			xml = xml.replaceFirst("^\\s*<\\?xml.*?\\?>", "");
		} catch (ParserException e) {
			return ResponseEntity.status(HttpStatus.UNPROCESSABLE_ENTITY).body(e.getMessage());
		}
		return ResponseEntity.ok().contentType(MediaType.APPLICATION_XML).body(xml);
	}

	@PostMapping(value = "/from", produces = MediaType.APPLICATION_OCTET_STREAM_VALUE, consumes = {
			MediaType.APPLICATION_XML_VALUE, MediaType.MULTIPART_FORM_DATA_VALUE })
	public ResponseEntity<String> fromXml(@RequestParam(required = false) MultipartFile file,
			@RequestBody(required = false) String body,
			@RequestParam(name = "areaAWidth", defaultValue = "6") int areaAWidth,
			@RequestParam(name = "nameStartCol", defaultValue = "8") int nameStartCol) {

		String content;
		try {
			content = extractContent(file, body);
			if (content == null) {
				return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("No input provided");
			}

			String result = renderCopybookFromXml(content, areaAWidth, nameStartCol);
			return ResponseEntity.ok().contentType(MediaType.APPLICATION_OCTET_STREAM).body(result);

		} catch (javax.xml.parsers.ParserConfigurationException | org.xml.sax.SAXException
				| javax.xml.transform.TransformerException | IOException e) {

			// return error message with 422 status
			return ResponseEntity.status(HttpStatus.UNPROCESSABLE_ENTITY).contentType(MediaType.TEXT_PLAIN)
					.body("XML processing error: " + e.getMessage());
		} catch (Exception e) {
			return ResponseEntity.status(HttpStatus.UNPROCESSABLE_ENTITY).contentType(MediaType.TEXT_PLAIN)
					.body("XML processing error: " + e.getMessage());

		}
	}

	private String extractContent(MultipartFile file, String body) throws IOException {
		if (file != null && !file.isEmpty()) {
			return new String(file.getBytes());
		} else if (body != null && !body.isBlank()) {
			return body;
		}
		return null;
	}

	private String renderCopybookFromXml(String xml, int areaAWidth, int nameStartCol) throws Exception {
		DocumentBuilder documentBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
		Document document = documentBuilder.parse(new ByteArrayInputStream(xml.getBytes()));

		TransformerFactory tf = TransformerFactory.newInstance();
		Transformer transformer = tf
				.newTransformer(new StreamSource(getClass().getResourceAsStream("/xslt/cpy-render.xsl")));

		// pass query params to XSLT
		transformer.setParameter("areaAWidth", areaAWidth);
		transformer.setParameter("nameStartCol", nameStartCol);

		StringWriter writer = new StringWriter();
		transformer.transform(new DOMSource(document), new StreamResult(writer));

		return writer.toString();
	}

	private String renderCopybookFromXml(String xml) throws Exception {
		DocumentBuilder documentBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
		Document document = documentBuilder.parse(new ByteArrayInputStream(xml.getBytes()));

		TransformerFactory tf = TransformerFactory.newInstance();
		Transformer transformer = tf
				.newTransformer(new StreamSource(getClass().getResourceAsStream("/xslt/cpy-render.xsl")));

		StringWriter writer = new StringWriter();
		transformer.transform(new DOMSource(document), new StreamResult(writer));

		return writer.toString();
	}

	@GetMapping(produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<String> hello() {
		return ResponseEntity.ok().build();
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
