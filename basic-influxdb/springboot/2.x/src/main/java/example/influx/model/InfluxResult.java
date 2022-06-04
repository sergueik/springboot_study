package example.influx.model;

import com.influxdb.annotations.Column;
import lombok.Data;

import java.time.Instant;

/**
 * description:
 * date: 2022/1/17 1:23
 * author: zhouhong
 */
@Data
public class InfluxResult {
    /**
     * 用电量
     */
    private String energyUsed;
    /**
     * 有功功率
     */
    private String power;
    /**
     * 电流
     */
    private String current;
    /**
     * 电压
     */
    private String voltage;
    /**
     * 时间戳
     */
    private String time;
}
