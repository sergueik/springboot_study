package example.domain;

// import lombok.Getter;
// import lombok.NoArgsConstructor;
// import lombok.Setter;

// @NoArgsConstructor
// @Getter
// @Setter
public class GrafanaTarget {
	public GrafanaTarget() {
	}

	String refId;
	String target;
	GrafanaTargetType type;

	public String getRefId() {
		return refId;
	}

	public void setRefId(String refId) {
		this.refId = refId;
	}

	public String getTarget() {
		return target;
	}

	public void setTarget(String target) {
		this.target = target;
	}

	public GrafanaTargetType getType() {
		return type;
	}

	public void setType(GrafanaTargetType type) {
		this.type = type;
	}

}
