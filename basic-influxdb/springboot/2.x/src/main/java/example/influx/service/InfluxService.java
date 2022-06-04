package example.influx.service;

import com.influxdb.query.FluxTable;

import example.influx.model.InsertParams;
import example.influx.model.InfluxResult;

import java.util.List;

public interface InfluxService {

	void insert(InsertParams insertParams);

	List<InfluxResult> queue();
}
