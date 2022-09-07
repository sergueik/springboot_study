package example.utils;

import java.lang.reflect.Type;
import java.util.Date;
import java.text.SimpleDateFormat;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;
import example.entity.DemoEntity;

// https://stackoverflow.com/questions/11038553/serialize-java-object-with-gson
public class DemoEntitySerializer implements JsonSerializer<DemoEntity> {

	private static final SimpleDateFormat simpleDateFormat = new SimpleDateFormat(
			"yyyy-MM-dd HH:mm:ss");

	@Override
	public JsonElement serialize(final DemoEntity data, final Type type,
			final JsonSerializationContext context) {
		JsonObject result = new JsonObject();

		final Date createTime = data.getCreateTime();
		if (createTime != null) {
			result.add("createTime",
					new JsonPrimitive(simpleDateFormat.format(createTime)));
		}

		int rpm = data.getRpm();
		if (rpm != 0) {
			result.add("rpm", new JsonPrimitive(rpm));
		}

		// NOTE: added static info from the serialized class
		// NPE ?
		/*
		if (type != null) {
			// java.lang.ClassCastException: java.lang.Class cannot be cast to example.entity.DemoEntity
			result.add("staticInfo",
					new JsonPrimitive(((DemoEntity) type).getStaticInfo()));
		} else {
			@SuppressWarnings("static-access")
			String staticInfo = data.getStaticInfo();
			System.err.println("Static info: " + staticInfo);
			if (staticInfo != null) {
				result.add("staticInfo", new JsonPrimitive(staticInfo));
			}
		}
		*/

		// filter what to (not) serialize
		@SuppressWarnings("unused")
		String environment = data.getEnvironment();

		String hostname = data.getHostname();
		if (hostname != null && !hostname.isEmpty()) {
			result.add("hostname", new JsonPrimitive(hostname));
		}
		String appId = data.getAppId();
		if (appId != null && !appId.isEmpty()) {
			result.add("appId", new JsonPrimitive(appId));
		}

		Float memory = data.getMemory();
		result.add("memory", new JsonPrimitive(memory));
		Float cpu = data.getCpu();
		result.add("cpu", new JsonPrimitive(cpu));

		return result;
	}
}
