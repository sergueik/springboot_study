package example.influx.config;

import com.influxdb.client.InfluxDBClient;
import com.influxdb.client.InfluxDBClientFactory;
import com.influxdb.client.WriteApi;
import lombok.Data;
import org.influxdb.InfluxDB;
import org.influxdb.InfluxDBFactory;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
@Data
@Configuration
@ConfigurationProperties(prefix = "influx")
public class Config {
    private String url;
    private String username;
    private String password;
    @Bean(destroyMethod = "close")
    public InfluxDB influxDBClient(){
        return InfluxDBFactory.connect(this.url, this.username, this.password);
    }
    @Bean(name = "influxDbWriteApi",destroyMethod = "close")
    public WriteApi influxDbWriteApi(){
        InfluxDBClient influxDBClient = InfluxDBClientFactory.createV1(this.url, this.username,
                this.password.toCharArray(), "influx_test", "autogen");
        return influxDBClient.getWriteApi();
    }
}
