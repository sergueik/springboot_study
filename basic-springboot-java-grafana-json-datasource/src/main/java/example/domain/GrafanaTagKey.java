package example.domain;

// import lombok.AllArgsConstructor;
// import lombok.Getter;
// import lombok.NoArgsConstructor;
// import lombok.Setter;

// @NoArgsConstructor
// @AllArgsConstructor
// @Getter
// @Setter
public class GrafanaTagKey {
	GrafanaTagType type;
	String text;

	public GrafanaTagKey() {
	}

	public GrafanaTagKey(GrafanaTagType type, String text) {
		super();
		this.type = type;
		this.text = text;
	}

	public GrafanaTagType getType() {
		return type;
	}

	public void setType(GrafanaTagType type) {
		this.type = type;
	}

	public String getText() {
		return text;
	}

	public void setText(String text) {
		this.text = text;
	}
}
