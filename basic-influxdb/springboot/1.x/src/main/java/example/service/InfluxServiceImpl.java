package example.service;

import com.influxdb.client.WriteApi;
import com.influxdb.client.domain.WritePrecision;

import example.influx.model.InsertParams;
import example.influx.model.InfluxResult;
import example.service.InfluxService;
import example.utils.InfluxUtil;
import lombok.extern.slf4j.Slf4j;
import org.influxdb.InfluxDB;
import org.influxdb.dto.QueryResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;

@Service
@Slf4j
public class InfluxServiceImpl implements InfluxService {
	@Resource(name = "influxDbWriteApi")
	private WriteApi influxDbWriteApi;
	@Resource(name = "influxDBClient")
	private InfluxDB influxDBClient;
	@Autowired
	private InfluxUtil influxUtil;

	@Override
	public void insert(InsertParams insertParams) {
		influxDbWriteApi.writeMeasurement(WritePrecision.MS, insertParams);
	}

	@Override
	public Object queryAll(InsertParams insertParams) {
		List<InfluxResult> list = new ArrayList<>();
		InfluxResult influxResult = new InfluxResult();
		String sql = "SELECT * FROM \"influx_test\" WHERE time > '2022-01-16'  tz('Asia/Shanghai')";
		QueryResult queryResult = influxUtil.query(sql, "influx_test",
				influxDBClient);
		System.err.println(queryResult.getResults().get(0).getSeries().get(0)
				.getValues().get(0).get(0));
		System.err.println(queryResult.getResults().get(0).getSeries().get(0)
				.getValues().get(0).get(1));
		queryResult.getResults().get(0).getSeries().get(0).getValues()
				.forEach(item -> {
					influxResult.setTime(item.get(0).toString());
					influxResult.setCurrent(item.get(1).toString());
					influxResult.setEnergyUsed(item.get(2).toString());
					influxResult.setPower(item.get(3).toString());
					influxResult.setVoltage(item.get(4).toString());
					list.add(influxResult);
				});
		return list;
	}

	@Override
	public Object querySumByOneDay(InsertParams insertParams) {
		String sql = "SELECT  SUM(voltage)  FROM \"influx_test\" WHERE time > '2022-01-18'  GROUP BY time(1d)  tz('Asia/Shanghai')";
		QueryResult queryResult = influxUtil.query(sql, "influx_test",
				influxDBClient);
		return queryResult.getResults().get(0).getSeries().get(0);
	}
}
