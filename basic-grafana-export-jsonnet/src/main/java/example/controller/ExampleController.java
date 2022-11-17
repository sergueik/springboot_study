package example.controller;
/**
 * Copyright 2022 Serguei Kouzmine
 */

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

// import example.model.grafana.List;
import example.model.grafana.AnnotationEntry;
import example.model.grafana.Annotations;
import example.model.grafana.Datasource;
import example.model.grafana.Root;
import example.utils.Utils;

import com.google.gson.FieldNamingPolicy;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import example.model.grafana.AnnotationEntry;

@RestController
@RequestMapping("/grafana")
public class ExampleController {

	// NOTE: cannot .setFieldNamingPolicy(FieldNamingPolicy.UPPER_CAMEL_CASE)
	// - the JSON fails to deserizliae when set
	private static final Gson gson = new GsonBuilder().setPrettyPrinting()
			.create();

	@SuppressWarnings("unchecked")
	@RequestMapping(method = RequestMethod.GET, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<?> grafana(

			@RequestParam(defaultValue = "") String version) {
		final String payload = Utils.getScriptContent("example.json");
		Root root = gson.fromJson(payload, Root.class);

		// thefollowing code demonstrates adding a single extra AnnotationEntry
		// commented - verifying round deserialization/serialization
		/*
		Annotations annotations = root.getAnnotations();
		AnnotationEntry annotationEntry = new AnnotationEntry();
		annotationEntry.setName("Annotations & Alerts");
		annotationEntry.setType("dashboard");
		annotationEntry.setEnable(true);
		annotationEntry.setHide(false);
		annotationEntry.setBuiltIn((float) 1.0);
		Datasource datasource = new Datasource();
		datasource.type = "grafana";
		datasource.uid = "-- Grafana --";
		annotationEntry.setDatasource(datasource);
		annotationEntry.setIconColor("rgba(0, 211, 255, 1)");
		
		List<AnnotationEntry> annotationEntryList = new ArrayList<>();
		annotationEntryList.add(annotationEntry);
		annotations.setList(annotationEntryList);
		root.setAnnotations(annotations);
		*/
		root.getPanels().stream().forEach(

				o -> {
					System.err.println(String.format("panel: %s", o.getType()));
					System.err.println(String.format("details: %s",
							o.getTargets().stream()
									.map(t -> String.format("expr:\"%s\", datasource: \"%s\"",
											t.getExpr(), t.getDatasource().getType()))
									.collect(Collectors.toList())));
				});

		// untyped Root
		Map<String, Object> untypedRoot = (Map<String, Object>) gson
				.fromJson(payload, Map.class);
		List<Object> untypedPanels = (List<Object>) untypedRoot.get("panels");
		Map<String, Object> untypedPanel = (Map<String, Object>) untypedPanels
				.get(0);
		System.err.println(String.format("panel: %s", untypedPanel.get("type")));
		List<Object> untypedPanelTargets = (List<Object>) untypedPanel
				.get("targets");
		Map<String, Object> untypedPanelTarget = (Map<String, Object>) untypedPanelTargets
				.get(0);
		Map<String, Object> untypedPanelTargetDatasource = (Map<String, Object>) untypedPanelTarget
				.get("datasource");
		System.err.println(String.format("details: %s",
				String.format("expr:\"%s\", datasource: \"%s\"",
						untypedPanelTarget.get("expr"),
						untypedPanelTargetDatasource.get("type"))));
		return ResponseEntity.ok().contentType(MediaType.APPLICATION_JSON)
				.body(root);

	}

}
