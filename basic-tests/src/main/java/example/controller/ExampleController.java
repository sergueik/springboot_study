package example.controller;
/**
 * Copyright 2021,2022 Serguei Kouzmine
 */

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.MultiValueMap;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import example.service.ExampleService;

@RestController
@RequestMapping("/basic")
public class ExampleController {

	private boolean debug = false;
	// see also about writing SpringBoot application tests without relying on
	// SpringBoot field injection
	// https://reflectoring.io/unit-testing-spring-boot/
	@Autowired
	private ExampleService service;

	@Autowired
	public ExampleController(ExampleService data) {
		service = data;
	}

	public ExampleController() {

	}

	@GetMapping
	public String hello() {
		return service.hello();
	}

	@GetMapping(value = "/json", produces = MediaType.APPLICATION_JSON_VALUE)
	public Data json() {
		return new Data(service.hello());
	}

	// NOTE:
	// @RequestMapping( produces =
	// MediaType.valueOf("application/pdf;charset=ASCII").toString())
	// leads to compile time error:
	// the value for annotation attribute RequestMapping.produces must be a
	// constant expression
	// see also: https://www.baeldung.com/spring-response-entity
	@RequestMapping(method = RequestMethod.GET, value = "/relevant_charset", produces = {
			MediaType.TEXT_PLAIN_VALUE })
	public ResponseEntity<String> returnRelevantCharsetPayload() {
		String data = "тест"; // TODO: localized string
		HttpHeaders headers = new HttpHeaders();
		// NOTE: default Content-Type is "text/plain; charset=us-ascii"
		headers.add("Content-Type", "text/plain;charset=UTF-8");
		return new ResponseEntity<String>(data, headers, HttpStatus.OK);
	}

	@RequestMapping(method = RequestMethod.GET, value = "/wrong_charset", produces = {
			MediaType.TEXT_PLAIN_VALUE })
	public ResponseEntity<String> returnGenericCharsetPayload() {
		String data = "тест"; // TODO: localized string
		HttpHeaders headers = new HttpHeaders();
		// NOTE: "Content-Type: text/plain; charset=us-ascii" is the default
		headers.add("Content-Type", "text/plain;charset=us-ascii");
		return new ResponseEntity<String>(data, headers, HttpStatus.OK);
	}

	// for downloading resources of the app
	// see also:
	// https://o7planning.org/11765/spring-boot-file-download
	// http://localhost:8085/basic/download/resource?resourceFileName=test.txt
	@RequestMapping(method = RequestMethod.GET, value = "/download/resource", produces = {
			MediaType.APPLICATION_OCTET_STREAM_VALUE })
	public ResponseEntity<?> downloadResource(
			@RequestParam(defaultValue = "test.txt") String resourceFileName) {
		// final String resourceFileName = "test.txt";
		Resource resource = null;
		try {
			URI uri = this.getClass().getClassLoader().getResource(resourceFileName)
					.toURI();
			resource = new UrlResource(uri);
			return ResponseEntity.ok().contentType(MediaType.APPLICATION_OCTET_STREAM)
					.header(HttpHeaders.CONTENT_DISPOSITION, String
							.format("attachment; filename=\"%s\"", resource.getFilename()))
					.body(resource);

		} catch (NullPointerException | IOException | URISyntaxException e) {
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
		}
	}

	// for files hosted on the system
	// alternative path value: "/download/file/{fileName:.+}"
	// see also:
	// https://www.javainuse.com/spring/boot-file-download
	// https://www.devglan.com/spring-boot/spring-boot-file-upload-download
	// http://www.mastertheboss.com/jboss-frameworks/resteasy/using-rest-services-to-manage-download-and-upload-of-files/

	@RequestMapping(method = RequestMethod.GET, value = "/download/file", produces = {
			MediaType.APPLICATION_OCTET_STREAM_VALUE })
	public ResponseEntity<?> downloadFile() {
		// NOTE: need an absolute path
		Path filePath = Paths.get(String.join(System.getProperty("file.separator"),
				System.getProperty("user.dir"), "src", "main", "resources",
				"test.txt"));
		Resource resource = null;
		try {
			File file = new File(filePath.toString());
			resource = new UrlResource(filePath.toUri());
			return ResponseEntity.ok().contentType(MediaType.APPLICATION_OCTET_STREAM)
					.header("Content-Length", Long.toString(file.length()))
					.header(HttpHeaders.CONTENT_DISPOSITION, String
							.format("attachment; filename=\"%s\"", resource.getFilename()))
					.body(resource);
		} catch (IOException e) {
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
					.body("file not found: " + filePath);
			// NOTE: code compiles but the body is ignored
			/*
			var data = new FileInputStream(filePath.toString());    
			IOUtils.copy(data, response.getOutputStream());
			IOUtils.closeQuietly(data);
			IOUtils.closeQuietly(response.getOutputStream());
			*/
		}
	}
	// see also:
	// testing upload
	// https://medium.com/red6-es/uploading-a-file-with-a-filename-with-spring-resttemplate-8ec5e7dc52ca
	// https://blogs.perficient.com/2022/07/12/api-testing-multipartfile-upload-using-java-spring-framework/

	// see also:
	// testing download (not ideal, checks body)
	// https://dev.to/tolgee_i18n/testing-file-download-in-spring-boot-3afc

	// TODO:
	// https://o7planning.org/11765/spring-boot-file-download
	// getMediaTypeForFileName

	@RequestMapping(method = RequestMethod.POST, value = "/post/json", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	public Data postJson(@RequestBody Data data) {
		@SuppressWarnings("unused")
		Data result = service.handleData(data);
		// TODO: return result leads to postJSONTest failure
		// return result;
		return data;
	}

	// see also: https://qna.habr.com/q/1079162
	@RequestMapping(value = "/list", method = RequestMethod.GET)
	public ResponseEntity<String> list(
			@RequestParam final Collection<UUID> uuids) {
		String data = String.join(" ",
				uuids.stream().map(o -> o.toString()).collect(Collectors.toList()));
		return ResponseEntity.status(HttpStatus.OK).body(data);
	}

	@RequestMapping(method = RequestMethod.POST, value = "/post/set", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Set<Data>> postSet(@RequestBody Set<String> inputs) {

		Set<Data> result = new HashSet<>();
		for (String input : inputs) {
			@SuppressWarnings("unused")
			Data data = new Data(input);
			result.add(data);
		}
		return ResponseEntity.status(HttpStatus.OK).body(result);

	}

	@GetMapping(value = "/array/params", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<List<String>> paramArrayEcho(
			@RequestParam Optional<List<String>> values) {
		return (values.isPresent() && values.get().size() > 0)
				? ResponseEntity.status(HttpStatus.OK).body(values.get())
				: ResponseEntity.status(HttpStatus.METHOD_NOT_ALLOWED)
						.body(new ArrayList<String>());
	}

	@GetMapping(value = "/queryparam", produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<String> queryParam(
			@RequestParam Optional<List<String>> appids,
			@RequestParam Optional<List<Integer>> ids) {
		String payload = null;
		if ((appids.isPresent() && appids.get().size() > 0)
				&& (ids.isPresent() && ids.get().size() > 0)) {
			payload = String.format("appids: %s ids: %s",
					String.join(",", appids.get()), String.join(",", ids.get().stream()
							.map(o -> String.format("%d", o)).collect(Collectors.toList())));
			return ResponseEntity.status(HttpStatus.OK).body(payload);
		} else {
			return ResponseEntity.status(HttpStatus.METHOD_NOT_ALLOWED).body("");

		}

	}

	// see also examples in
	// https://www.programcreek.com/java-api-examples/?class=org.springframework.http.MediaType&method=APPLICATION_FORM_URLENCODED_VALUE
	// https://www.baeldung.com/spring-request-method-not-supported-405
	// returns HTTP 405 error code for GET
	@RequestMapping(method = { RequestMethod.PUT,
			RequestMethod.POST }, value = "/post/form", consumes = MediaType.APPLICATION_FORM_URLENCODED_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Data> postForm(
			@RequestBody final MultiValueMap<String, String> param /* , HttpServletResponse response */) {
		if (param.isEmpty()) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(new Data());
			// Alternatively change the method signature to include
			// HttpServletResponse response
			// and then
			// response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
			// see also:
			// https://stackoverflow.com/questions/16232833/how-to-respond-with-http-400-error-in-a-spring-mvc-responsebody-method-returnin
		}
		return ResponseEntity.status(HttpStatus.OK)
				.body(service.handleData(new Data(param.getFirst("name"))));
	}

	// NOTE: getResourceURI may not work with standalone or web hosted
	// application
	public String getResourceURI(String resourceFileName) {
		//
		try {
			if (debug) {
				System.err.println("Getting resource URI for: " + resourceFileName);
			}

			URI uri = this.getClass().getClassLoader().getResource(resourceFileName)
					.toURI();
			if (debug) {
				System.err.println("Resource URI: " + uri.toString());
			}
			return uri.toString();
		} catch (URISyntaxException e) {
			throw new RuntimeException(e);
		}
	}

	@GetMapping(value = "/servererror", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<String> serverError() {
		return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("");
	}

	public static class Data {

		private String name;

		public String getName() {
			return name;
		}

		public void setName(String data) {
			name = data;
		}

		public Data(String name) {
			this.name = name;
		}

		public Data() {
		}
	}
}
