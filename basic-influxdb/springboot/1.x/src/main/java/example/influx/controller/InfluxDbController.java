package example.influx.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import example.influx.model.InsertParams;
import example.influx.model.ResponseData;
import example.influx.model.SuccessResponseData;
import example.service.InfluxService;

@RestController
public class InfluxDbController {
	@Autowired
	private InfluxService influxService;

	@PostMapping("/influxdb/insert")
	public ResponseData insert(@RequestBody InsertParams insertParams) {
		influxService.insert(insertParams);
		return new SuccessResponseData();
	}

	@PostMapping("/influxdb/queryAll")
	public ResponseData query(@RequestBody InsertParams insertParams) {
		return new SuccessResponseData(influxService.queryAll(insertParams));
	}

	@PostMapping("/influxdb/queryByOneDay")
	public ResponseData queryByOneDay(@RequestBody InsertParams insertParams) {
		return new SuccessResponseData(
				influxService.querySumByOneDay(insertParams));
	}
}
