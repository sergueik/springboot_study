package example.service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Iterator;
import java.util.Map;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

@Service
public class JsonAssembler {

	private final ObjectMapper mapper = new ObjectMapper();

	@Value("${app.components.customer}")
	private Resource customer;

	@Value("${app.components.account}")
	private Resource account;

	@Value("${app.components.transaction}")
	private Resource transaction;

	public JsonNode assemble() throws Exception {
		JsonNode node = load(transaction.getFilename());
		return assemble(node);
	}
	// interprets only two tokens:
	// "$ref"
	// "anyOf"
	public JsonNode assemble(JsonNode node) throws Exception {

		if (!node.isObject())
			return node;

		ObjectNode obj = (ObjectNode) node;

		// 1. handle $ref
		if (obj.has("$ref")) {
			String ref = obj.get("$ref").asText();
			return assemble(load(ref));
		}

		// 2. handle anyOf (VERY simple rule: pick first)
		if (obj.has("anyOf")) {
			JsonNode first = obj.get("anyOf").get(0);
			return assemble(first);
		}

		// 3. recurse into fields
		Iterator<Map.Entry<String, JsonNode>> it = obj.fields();
		while (it.hasNext()) {
			Map.Entry<String, JsonNode> e = it.next();
			obj.set(e.getKey(), assemble(e.getValue()));
		}

		return obj;
	}

	private Path baseDir = Paths.get("src/main/resources/components");

	private JsonNode load(String name) throws IOException {
		Path file = baseDir.resolve(name);
		return mapper.readTree(Files.newInputStream(file));
	}

}
