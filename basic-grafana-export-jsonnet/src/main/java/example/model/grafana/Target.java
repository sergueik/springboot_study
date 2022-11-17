package example.model.grafana;

import java.util.ArrayList;
import java.util.List;

// generated via https://json2csharp.com/code-converters/json-to-pojo
public class Target {

	public String refId;
	public String target;
	public String type;
	private List<String> tags = new ArrayList<>();
	private int limit;
	private boolean matchAny;

	public int getLimit() {
		return limit;
	}

	public void setLimit(int data) {
		limit = data;
	}

	public boolean isMatchAny() {
		return matchAny;
	}

	public void setMatchAny(boolean data) {
		matchAny = data;
	}

	public List<String> getTags() {
		return tags;
	}

	public void setTags(List<String> data) {
		tags = data;
	}

	public String getRefId() {
		return refId;
	}

	public void setRefId(String data) {
		refId = data;
	}

	public String getTarget() {
		return target;
	}

	public void setTarget(String data) {
		target = data;
	}

	public String getType() {
		return type;
	}

	public void setType(String data) {
		type = data;
	}

}
