package example.domain;

// import lombok.Getter;
// import lombok.NoArgsConstructor;
// import lombok.Setter;

import java.time.Instant;

// @NoArgsConstructor
// @Setter
// @Getter
public class GrafanaRange {
	Instant from;
	Instant to;

	public GrafanaRange() {
	}

	public Instant getFrom() {
		return from;
	}

	public void setFrom(Instant from) {
		this.from = from;
	}

	public Instant getTo() {
		return to;
	}

	public void setTo(Instant to) {
		this.to = to;
	}
}
