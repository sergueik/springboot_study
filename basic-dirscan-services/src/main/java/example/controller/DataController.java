package example.controller;

import java.io.BufferedReader;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.lang.IllegalArgumentException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import example.model.DataRow;
import example.model.HostDataRow;
import example.service.ServerService;

@Controller
@RequestMapping("/")
public class DataController {
	private boolean debug = false;

	public void setDebug(boolean data) {
		debug = data;
	}

	private Log log = LogFactory.getLog(this.getClass());

	private String baseDirectory = System.getProperty("os.name").toLowerCase()
			.contains("windows") ? System.getenv("TEMP") : "/tmp";

	/*
	private final ServerService service;
	
	@Autowired
	public DataController(ServerService data) {
	service = data;
	}
	*/
	@ResponseBody
	@GetMapping(value = { "/server/{name}" })
	public ResponseEntity<List<String>> showServers(@PathVariable String name) {
		List<String> data = new ArrayList<>();
		try {
			ServerService serverService = new ServerService(name);
			data = serverService.getServers();
			return ResponseEntity.status(HttpStatus.OK)
					.contentType(MediaType.APPLICATION_JSON_UTF8).body(data);
		} catch (RuntimeException e) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST)
					.body(Arrays.asList(e.getMessage()));
		}
	}
}
