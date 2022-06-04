package example.influx.config;

import com.influxdb.client.InfluxDBClient;
import com.influxdb.client.InfluxDBClientFactory;
import com.influxdb.client.WriteApi;
import lombok.Data;
import org.influxdb.InfluxDB;
import org.influxdb.InfluxDBFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Data
@Configuration
@ConfigurationProperties(prefix = "influx")
public class Config {
	private String influxUrl;
	private String bucket;
	private String org;
	private String token;

	@Bean(name = "influx")
	public InfluxBean InfluxBean() {
		return new InfluxBean(influxUrl, bucket, org, token);
	}
}