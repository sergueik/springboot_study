package example.layouts;

import org.apache.log4j.Layout;
import org.apache.log4j.spi.LoggingEvent;
import org.apache.log4j.spi.ThrowableInformation;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

public class JSONLayout extends Layout {

	@Override
	public String format(LoggingEvent loggingEvent) {

		JSONObject root = new JSONObject();

		try {
			writeBasic(root, loggingEvent);

			writeThrowable(root, loggingEvent);

		} catch (JSONException e) {
			e.printStackTrace();
		}

		return root.toString();
	}

	/**
	 * Converts LoggingEvent Throwable to JSON object
	 * 
	 * @param json
	 * @param event
	 * @throws JSONException
	 */
	protected void writeThrowable(JSONObject json, LoggingEvent event) throws JSONException {
		ThrowableInformation ti = event.	getThrowableInformation();
		if (ti != null) {
			Throwable t = ti.getThrowable();
			JSONObject throwable = new JSONObject();

			throwable.put("message", t.getMessage());
			throwable.put("className", t.getClass().getCanonicalName());
			List<JSONObject> traceObjects = new ArrayList<JSONObject>();
			for (StackTraceElement ste : t.getStackTrace()) {
				JSONObject element = new JSONObject();
				element.put("class", ste.getClassName());
				element.put("method", ste.getMethodName());
				element.put("line", ste.getLineNumber());
				element.put("file", ste.getFileName());
				traceObjects.add(element);
			}

			json.put("stackTrace", traceObjects);
			json.put("throwable", throwable);
		}
	}

	/**
	 * Converts basic LogginEvent properties to JSON object
	 * 
	 * @param json
	 * @param event
	 * @throws JSONException
	 */
	protected void writeBasic(JSONObject json, LoggingEvent event) throws JSONException {
		json.put("threadName", event.getThreadName());
		json.put("level", event.getLevel().toString());
		json.put("timestamp", event.getTimeStamp());
		json.put("message", event.getMessage());
		json.put("logger", event.getLoggerName());
	}

	@Override
	public boolean ignoresThrowable() {
		return false;
	}

	@Override
	public void activateOptions() {
	}

}
