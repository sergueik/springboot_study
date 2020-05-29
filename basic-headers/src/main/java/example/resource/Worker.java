package example.resource;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.net.URI;
import java.util.Map;

@SuppressWarnings("unused")
@Component
@RestController
@RequestMapping("/headers")
public class Worker {

	@GetMapping("/typed")
	public ResponseEntity<String> listAllHeaders(@RequestHeader HttpHeaders headers) {
		StringBuffer headerData = new StringBuffer();
		headers.forEach((key, values) -> headerData.append(String.format("Header \"%s\" = %s\n", key, values.get(0))));
		// for the host need 5.6
		// https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/http/HttpHeaders.html
		// headers).getHost();
		return new ResponseEntity<String>(String.format("Headers:\n%s", headerData.toString()), HttpStatus.OK);
	}

	@GetMapping("/map")
	public ResponseEntity<String> listAllHeaders(@RequestHeader Map<String, String> headers) {
		StringBuffer headerData = new StringBuffer();
		headers.forEach((key, value) -> {
			headerData.append(String.format("Header \"%s\" = %s\n", key, value));
		});
		return new ResponseEntity<String>(String.format("Headers:\n%s", headerData.toString()), HttpStatus.OK);
	}

	@GetMapping("/check")
	public ResponseEntity<String> getOptionalHeader(
			@RequestHeader(value = "optional", required = false) String optionalHeader) {
		return new ResponseEntity<String>(
				String.format("The optional header was %s", (optionalHeader == null ? "absent" : "present")),
				HttpStatus.OK);
	}
}
