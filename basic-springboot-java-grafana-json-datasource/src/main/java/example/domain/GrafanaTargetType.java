package example.domain;

import com.fasterxml.jackson.annotation.JsonProperty;

public enum GrafanaTargetType {
	@JsonProperty("timeserie") TIMESERIE,
	@JsonProperty("table") TABLE;
}
