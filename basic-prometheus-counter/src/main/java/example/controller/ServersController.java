package example.controller;
/**
 * Copyright 2022 Serguei Kouzmine
 */

import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import example.dao.JDBCDao;

/*
 *  @author: Serguei Kouzmine (kouzmine_serguei@yahoo.com)
 */

@RestController
@RequestMapping("/")
public class ServersController {

	private static final Logger logger = LogManager
			.getLogger(ServersController.class);

	@Autowired
	private JDBCDao dao;
	
	private static final boolean debug = false;

	@ResponseBody
	@GetMapping(value = "servers", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<List<?>> servers() {
		if (debug)
			logger.info("Starting reporting servers");
		List<?> payload = dao.findAllServerInstanceApplication();

		return (payload == null)
				? ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null)
				: ResponseEntity.status(HttpStatus.OK).body(payload);
	}

}
