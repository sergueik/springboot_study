package example.controllers;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import example.models.ExampleModel;
import example.repository.ExampleRepository;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@RestController
public class SampleController {

	@Autowired
	private ExampleRepository dao;

	private static Logger logger = LoggerFactory
			.getLogger(SampleController.class);

	@GetMapping("/data")
	public List<ExampleModel> getData() {
		List<ExampleModel> data = dao.getTestData();
		logger.info("found: {} objects", data.size());
		return data;
	}
}
