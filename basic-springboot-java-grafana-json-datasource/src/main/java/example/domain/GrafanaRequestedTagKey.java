package example.domain;

// import lombok.AllArgsConstructor;
// import lombok.Getter;
// import lombok.NoArgsConstructor;
// import lombok.Setter;

// @NoArgsConstructor
// @AllArgsConstructor
// @Getter
// @Setter
public class GrafanaRequestedTagKey {
	String key;

	public GrafanaRequestedTagKey() {
	}

	public GrafanaRequestedTagKey(String key) {
		super();
		this.key = key;
	}

	public String getKey() {
		return key;
	}

	public void setKey(String key) {
		this.key = key;
	}
}
