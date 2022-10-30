package example.domain;

// import lombok.AllArgsConstructor;
// import lombok.Getter;
// import lombok.NoArgsConstructor;
// import lombok.Setter;

// @NoArgsConstructor
// @AllArgsConstructor
// @Getter
// @Setter
public class GrafanaTagValue {

	private String text;

	public GrafanaTagValue() {
	}

	public String getText() {
		return text;
	}

	public void setText(String data) {
		text = data;
	}

	public GrafanaTagValue(String text) {
		this.text = text;
	}
}
