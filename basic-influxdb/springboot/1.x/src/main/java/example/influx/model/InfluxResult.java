package example.influx.model;

import com.influxdb.annotations.Column;
import lombok.Data;

import java.time.Instant;

@Data
public class InfluxResult {
    public String getEnergyUsed() {
		return energyUsed;
	}
	public void setEnergyUsed(String energyUsed) {
		this.energyUsed = energyUsed;
	}
	public String getPower() {
		return power;
	}
	public void setPower(String power) {
		this.power = power;
	}
	public String getCurrent() {
		return current;
	}
	public void setCurrent(String current) {
		this.current = current;
	}
	public String getVoltage() {
		return voltage;
	}
	public void setVoltage(String voltage) {
		this.voltage = voltage;
	}
	public String getTime() {
		return time;
	}
	public void setTime(String time) {
		this.time = time;
	}
		private String energyUsed;
    private String power;
    private String current;
    private String voltage;
    private String time;
}
