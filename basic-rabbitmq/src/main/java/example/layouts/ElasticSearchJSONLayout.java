package example.layouts;

import org.apache.log4j.Layout;
import org.apache.log4j.spi.LoggingEvent;
import org.apache.log4j.spi.ThrowableInformation;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

public class ElasticSearchJSONLayout extends JSONLayout {

	private String index = "json-index";
	private String type = "json";

	/**
	 * format a given LoggingEvent to a string, in this case JSONified string
	 * 
	 * @param loggingEvent
	 * @return String representation of LoggingEvent
	 */
	@Override
	public String format(LoggingEvent loggingEvent) {

		StringBuilder sb = new StringBuilder();

		JSONObject action = new JSONObject();
		JSONObject source = new JSONObject();

		try {
			JSONObject actionContent = new JSONObject();
			actionContent.put("_index", this.index);
			actionContent.put("_type", this.type);
			action.put("index", actionContent);

			JSONObject sourceContent = new JSONObject();

			writeBasic(sourceContent, loggingEvent);

			writeThrowable(sourceContent, loggingEvent);

			source.put(this.type, sourceContent);

		} catch (JSONException e) {
			e.printStackTrace();
		}

		sb.append(action.toString());
		sb.append("\n");
		sb.append(source.toString());
		sb.append("\n");

		return sb.toString();

	}

	public String getIndex() {
		return index;
	}

	public void setIndex(String value) {
		index = value;
	}

	public String getType() {
		return type;
	}

	public void setType(String value) {
		type = value;
	}

	@Override
	public boolean ignoresThrowable() {
		return false;
	}

	@Override
	public void activateOptions() {

	}

}
