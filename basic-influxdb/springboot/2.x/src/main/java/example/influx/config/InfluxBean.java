package example.influx.config;

import com.influxdb.client.InfluxDBClient;
import com.influxdb.client.InfluxDBClientFactory;
import com.influxdb.client.WriteApi;
import com.influxdb.client.domain.WritePrecision;
import com.influxdb.query.FluxTable;
import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Setter
@Getter
public class InfluxBean {
	private String influxUrl;
	private String bucket;
	private String org;
	private String token;
	private InfluxDBClient client;

	public InfluxBean(String influxUrl, String bucket, String org, String token) {
		this.influxUrl = influxUrl;
		this.bucket = bucket;
		this.org = org;
		this.token = token;
		this.client = getClient();
	}

	private InfluxDBClient getClient() {
		if (client == null) {
			client = InfluxDBClientFactory.create(influxUrl, token.toCharArray());
		}
		return client;
	}

	public void write(Object object) {
		try (WriteApi writeApi = client.getWriteApi()) {
			writeApi.writeMeasurement(bucket, org, WritePrecision.NS, object);
		}
	}

	public List<FluxTable> queryTable(String fluxQuery) {
		return client.getQueryApi().query(fluxQuery, org);
	}
}