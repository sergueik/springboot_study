package example.influx.model;

import com.influxdb.annotations.Column;
import com.influxdb.annotations.Measurement;
import lombok.Data;

import java.time.Instant;

@Data
// https://javadoc.io/static/org.influxdb/influxdb-java/2.20/org/influxdb/annotation/Measurement.html
@Measurement(name = "influx_test")
public class InsertParams {
	@Column
	private Double energyUsed;

	@Column(tag = true)
	private Double power;

	@Column
	private Double current;

	@Column
	private Double voltage;

	@Column(timestamp = true)
	private Instant time;
}
