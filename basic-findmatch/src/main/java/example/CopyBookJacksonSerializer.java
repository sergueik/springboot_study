package example;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import java.io.IOException;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

public class CopyBookJacksonSerializer extends JsonSerializer<Map<String, Object>> {

// NOTE:  using Parameterized type/fully typed map 
// make the code self-documenting by giving the parameter a “loud” descriptive name matching its type
    @Override
    public void serialize( Map<String, Object> value, JsonGenerator jsonGenerator, SerializerProvider unusedProvider) throws IOException {

        jsonGenerator.writeStartObject();

        // Inject synthetic metadata for audit / tracing
        jsonGenerator.writeStringField("owner_uuid", UUID.randomUUID().toString());

        // filter fields
        Set<String> excludeFields = Set.of(
                "EIBCALEN", "EIBTIME", "EIBTRNID", "CURSOR_POS"
        );

        for (Map.Entry<String, Object> entry : value.entrySet()) {
            if (!excludeFields.contains(entry.getKey())) {
                jsonGenerator.writeObjectField(entry.getKey(), entry.getValue());
            }
        }

        jsonGenerator.writeEndObject();
    }
}

