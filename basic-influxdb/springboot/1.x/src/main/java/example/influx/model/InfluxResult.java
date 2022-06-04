package example.influx.model;

import com.influxdb.annotations.Column;
import lombok.Data;

import java.time.Instant;

@Data
public class InfluxResult {
    private String energyUsed;
    private String power;
    private String current;
    private String voltage;
    private String time;
}
