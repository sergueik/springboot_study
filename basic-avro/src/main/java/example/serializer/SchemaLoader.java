package example.serializer;

import org.apache.avro.Schema;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class SchemaLoader {
	public Schema load(String avscPath, String schemaName) throws IOException {
		String content = Files.readString(Path.of(avscPath)).trim();
		return new Schema.Parser().parse(content);
	}
}
