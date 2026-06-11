package example.dto;

import java.lang.reflect.Type;
import java.util.Base64;

import com.google.gson.FieldNamingStrategy;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;

// https://stackoverflow.com/questions/11038553/serialize-java-object-with-gson
public class UploadRequestSerializer implements JsonSerializer<UploadRequest> {
	@Override
	public JsonElement serialize(final UploadRequest data, final Type type, final JsonSerializationContext context) {
		JsonObject result = new JsonObject();
		int id = data.getId();
		if (id != 0) {
			result.add("id", new JsonPrimitive(id));
		}
		// added static info from the serialized class
		// NPE
		if (type != null) {
			result.add("staticInfo", new JsonPrimitive(((UploadRequest) type).getStaticInfo()));
		} else {
			String staticInfo = data.getStaticInfo();
			System.err.println("Static info: " + staticInfo);
			if (staticInfo != null) {
				result.add("staticInfo", new JsonPrimitive(staticInfo));
			}
		}

		@SuppressWarnings("unused")
		String foo = data.getFoo();
		// filter what to (not) serialize

		String bar = data.getBar();
		if (bar != null && !bar.isEmpty()) {
			result.add("bar", new JsonPrimitive(bar));
		}
		/*
		 * Float price = data.getPrice(); result.add("price", new JsonPrimitive(price));
		 */
		return result;
	}
}
