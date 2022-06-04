package example.influx.model;

import com.influxdb.annotations.Column;
import com.influxdb.annotations.Measurement;
import lombok.Data;

import java.time.Instant;

/**
 * description: 时序数据库添加参数
 * date: 2022/1/16 20:40
 * author: zhouhong
 */
@Data
@Measurement(name = "influx_test")
public class InsertParams {
    /**
      * 用电量
      */
    @Column(tag = true)
    private String energyUsed;

    /**
     * 有功功率
     */
    @Column
    private String  power;

    /**
     * 电流
     */
    @Column
    private String current;
    /**
     * 电压
     */
    @Column
    private String voltage;
    /**
     * 时间戳
     */
    @Column(timestamp = true)
    private Instant time;
}
