package example.controller;
/**
 * Copyright 2022 Serguei Kouzmine
 */

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import example.model.grafana.AnnotationEntry;
import example.model.grafana.Annotations;
import example.model.grafana.Root;
import example.utils.Utils;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

@RestController
@RequestMapping("/grafana")
public class ExampleController {

	private static final Gson gson = new GsonBuilder().setPrettyPrinting()
			.create();

	@RequestMapping(method = RequestMethod.GET, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<?> grafana(

			@RequestParam(defaultValue = "") String version) {
		final String payload = Utils.getScriptContent("example.json");
		Root root = gson.fromJson(payload, Root.class);
		
		Annotations annotations = new Annotations();
		AnnotationEntry annotationEntry = new AnnotationEntry();
		annotationEntry.setName("Annotations & Alerts");
		annotationEntry.setType("dashboard");
		annotationEntry.setEnable(true);
		annotationEntry.setHide(false);
		annotationEntry.setBuiltIn((float) 1.0);
		annotationEntry.setDatasource("grafana");
		annotationEntry.setIconColor("rgba(0, 211, 255, 1)");

		List<AnnotationEntry> annotationEntryList = new ArrayList<>();
		annotationEntryList.add(annotationEntry);
		annotations.setAnnotationEntries(annotationEntryList);
		root.annotations = annotations;
		return ResponseEntity.ok().contentType(MediaType.APPLICATION_JSON)
				.body(root);
	}

}
