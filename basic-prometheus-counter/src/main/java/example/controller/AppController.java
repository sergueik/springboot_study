package example.controller;

/**
 * Copyright 2022 Serguei Kouzmine
 */

import org.springframework.web.bind.annotation.RestController;

import example.entity.Host;
import example.utils.HostData;
import example.entity.Result;
import example.service.BaseService;
import example.utils.ClusterConfigReader;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import io.prometheus.client.Collector.MetricFamilySamples;
import io.prometheus.client.CollectorRegistry;
import io.prometheus.client.Counter;
import io.prometheus.client.Gauge;
import io.prometheus.client.Gauge.Builder;
import io.prometheus.client.Histogram;
import io.prometheus.client.exporter.common.TextFormat;

// import io.micrometer.core.annotation.Timed;
// import io.micrometer.core.instrument.Counter;
// import io.micrometer.core.instrument.MeterRegistry;

import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

import javax.annotation.Resource;

import java.io.IOException;
import java.io.StringWriter;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.Level;
import org.sqlite.SQLiteException;

@RestController
@RequestMapping("/")
public class AppController {

	// index page
	@GetMapping(produces = MediaType.TEXT_HTML_VALUE)
	@ResponseBody
	public ResponseEntity<String> index() {
		String body = "<html>" + "<head>" + "</head>" + "<body>"
				+ "<a href=\"./metrics\"\">Application hosted metrics REST service</a><br/>"
				+ "</body>" + "</html>";
		return ResponseEntity.ok().body(body);
	}

}
