package example.influx.service;

import com.influxdb.client.InfluxDBClient;
import com.influxdb.client.InfluxDBClientFactory;
import com.influxdb.query.FluxRecord;
import com.influxdb.query.FluxTable;

import example.influx.config.InfluxBean;
import example.influx.model.InsertParams;
import example.influx.model.InfluxResult;
import example.influx.service.InfluxService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

@Service
@Slf4j
public class InfluxServiceImpl implements InfluxService {

	@Resource
	private InfluxBean influxBean;

	@Override
	public void insert(InsertParams insertParams) {
		insertParams.setTime(Instant.now());
		influxBean.write(insertParams);
	}

	@Override
	public List<InfluxResult> queue() {
		List<FluxTable> list = queryInfluxAll();
		List<InfluxResult> results = new ArrayList<>();
		for (int i = 0; i < list.size(); i++) {
			for (int j = 0; j < list.get(i).getRecords().size(); j++) {
				InfluxResult influxResult = new InfluxResult();
				influxResult.setCurrent(list.get(i).getRecords().get(j).getValues()
						.get("current").toString());
				influxResult.setEnergyUsed(list.get(i).getRecords().get(j).getValues()
						.get("energyUsed").toString());
				influxResult.setPower(list.get(i).getRecords().get(j).getValues()
						.get("power").toString());
				influxResult.setVoltage(list.get(i).getRecords().get(j).getValues()
						.get("voltage").toString());
				influxResult.setTime(list.get(i).getRecords().get(j).getValues()
						.get("_time").toString());
				System.err
						.println(list.get(i).getRecords().get(j).getValues().toString());
				results.add(influxResult);
			}
		}
		return results;
	}

	private List<FluxTable> queryInfluxAll() {
		String query = " from(bucket: \"tom\")"
				+ "  |> range(start: -60m, stop: now())"
				+ "  |> filter(fn: (r) => r[\"_measurement\"] == \"influx_test\")"
				+ "  |> pivot( rowKey:[\"_time\"], columnKey: [\"_field\"], valueColumn: \"_value\" )";
		return influxBean.queryTable(query);
	}

	public List<FluxTable> queryFilterByEnergyUsed() {
		String query = " from(bucket: \"tom\")"
				+ "  |> range(start: -60m, stop: now())"
				+ "  |> filter(fn: (r) => r[\"_measurement\"] == \"influx_test\")"
				+ "  |> filter(fn: (r) => r[\"energyUsed\"] == \"322\")"
				+ "  |> pivot( rowKey:[\"_time\"], columnKey: [\"_field\"], valueColumn: \"_value\" )";
		return influxBean.queryTable(query);
	}
}
