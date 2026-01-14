package example;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import java.io.IOException;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

public class CopyBookJacksonSerializer extends JsonSerializer<CopyBook> {

	private final Set<String> excludeKeys;
	private final Set<String> hardCodedExcludedKeys = Set.of("EIBCALEN", "EIBTIME", "EIBTRNID", "CURSOR_POS");

	public CopyBookJacksonSerializer(Set<String> excludeKeys) {
		this.excludeKeys = new HashSet<>(hardCodedExcludedKeys);

		if (excludeKeys != null) {
			// Only add keys not already present (HashSet automatically handles duplicates)
			for (String key : excludeKeys) {
				this.excludeKeys.add(key);
			}
		}
	}

	public CopyBookJacksonSerializer() {
		this.excludeKeys = new HashSet<>(hardCodedExcludedKeys);
	}

    @Override
    public Class<CopyBook> handledType() {
        return CopyBook.class;
    }
    // NOTE: using Parameterized type/fully typed map 
	@Override
	public void serialize(CopyBook  value, JsonGenerator jsonGenerator, SerializerProvider unusedProvider)
			throws IOException {

		jsonGenerator.writeStartObject();

		// Inject synthetic metadata for audit / tracing
		jsonGenerator.writeStringField("owner_uuid", UUID.randomUUID().toString());

		for (Map.Entry<String, String> entry : value.entrySet()) {
			if (!this.excludeKeys.contains(entry.getKey())) {
				jsonGenerator.writeObjectField(entry.getKey(), entry.getValue());
			}
		}

		jsonGenerator.writeEndObject();
	}
}
