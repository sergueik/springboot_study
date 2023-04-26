package example.model;

import java.lang.reflect.Type;
import com.google.gson.FieldNamingStrategy;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;

import org.json.JSONException;
import org.json.JSONObject;
import org.json.XML;

import example.model.Artist;

// https://stackoverflow.com/questions/11038553/serialize-java-object-with-gson
public class ArtistSerializer implements JsonSerializer<Artist> {
	@Override
	public JsonElement serialize(final Artist data, final Type type,
			final JsonSerializationContext context) {
		JsonObject result = new JsonObject();
		int id = data.getId();
		if (id != 0) {
			result.add("id", new JsonPrimitive(id));
		}
		// added static info from the serialized class
		// NPE
		if (type != null) {
			result.add("staticInfo",
					new JsonPrimitive(((Artist) type).getStaticInfo()));
		} else {
			String staticInfo = data.getStaticInfo();
			System.err.println("Static info: " + staticInfo);
			if (staticInfo != null) {
				result.add("staticInfo", new JsonPrimitive(staticInfo));
			}
		}

		@SuppressWarnings("unused")
		String name = data.getName();
		// filter what to (not) serialize

		String plays = data.getPlays();
		if (plays != null && !plays.isEmpty()) {
			result.add("plays", new JsonPrimitive(plays));
		}
		/*
		 * Float price = data.getPrice(); result.add("price", new
		 * JsonPrimitive(price));
		 */
		return result;
	}

}
