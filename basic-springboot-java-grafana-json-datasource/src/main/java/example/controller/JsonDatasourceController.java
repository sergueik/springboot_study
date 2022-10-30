package example.controller;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.http.ResponseEntity;
// import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import com.fasterxml.jackson.databind.ObjectMapper;

import example.domain.GrafanaQuery;
import example.domain.GrafanaRequestedTagKey;
import example.domain.GrafanaTagKey;
import example.domain.GrafanaTagType;
import example.domain.GrafanaTagValue;
import example.domain.GrafanaTarget;

@Controller
public class JsonDatasourceController {
	@Value("classpath*:data/*.json")
	private Resource[] metrics;

	private ObjectMapper jsonMapper = new ObjectMapper();

	@GetMapping("/")
	public ResponseEntity<?> testConnection() {
		return ResponseEntity.ok().build();
	}

	@PostMapping("/search")
	public ResponseEntity<List<String>> search() {
		List<String> metricNames = Arrays.stream(metrics).map(Resource::getFilename)
				.collect(Collectors.toList());
		return ResponseEntity.ok(metricNames);
	}

	@PostMapping("/query")
	public ResponseEntity<List<Object>> query(@RequestBody GrafanaQuery query
	/* , Authentication authentication */) throws IOException {
		List<String> requestedTargetNames = query.getTargets().stream()
				.map(GrafanaTarget::getTarget).collect(Collectors.toList());

		List<Object> metrics = Arrays.stream(this.metrics)
				.filter(metric -> requestedTargetNames.contains(metric.getFilename()))
				.map(metric -> {
					try {
						return metric.getFile();
					} catch (IOException e) {
						e.printStackTrace();
						return null;
					}
				}).map(file -> {
					try {
						return jsonMapper.readValue(file, Object.class);
					} catch (IOException e) {
						e.printStackTrace();
						return null;
					}
				}).collect(Collectors.toList());

		return ResponseEntity.ok(metrics);
	}

	@PostMapping("/annotations")
	public ResponseEntity<List<Object>> annotations(
			@RequestBody GrafanaQuery query/* , Authentication authentication */)
			throws IOException {
		// TODO implement
		return ResponseEntity.ok(new ArrayList<>());
	}

	@PostMapping("/tag-keys")
	public ResponseEntity<List<GrafanaTagKey>> tagKeys(
	/* Authentication authentication */) throws IOException {
		List<GrafanaTagKey> keys = new ArrayList<>();
		keys.add(new GrafanaTagKey(GrafanaTagType.STRING, "key1"));
		keys.add(new GrafanaTagKey(GrafanaTagType.STRING, "key2"));

		return ResponseEntity.ok(keys);
	}

	@PostMapping("/tag-values")
	public ResponseEntity<List<GrafanaTagValue>> tagValues(
			@RequestBody GrafanaRequestedTagKey key/* , Authentication authentication */)
			throws IOException {
		List<GrafanaTagValue> keys = new ArrayList<>();
		keys.add(new GrafanaTagValue("value 1 " + key.getKey()));
		keys.add(new GrafanaTagValue("value 2 " + key.getKey()));

		return ResponseEntity.ok(keys);
	}
}
