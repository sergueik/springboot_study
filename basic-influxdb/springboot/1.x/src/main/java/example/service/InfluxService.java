package example.service;

import example.influx.model.InsertParams;

public interface InfluxService {
	void insert(InsertParams insertParams);
	Object queryAll(InsertParams insertParams);
	Object querySumByOneDay(InsertParams insertParams);
}
