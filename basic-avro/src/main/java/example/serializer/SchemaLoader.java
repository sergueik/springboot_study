package example.serializer;

import org.apache.avro.Schema;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Loads Avro schemas from .avsc files.
 */
public class SchemaLoader {

    /**
     * Load a schema from an .avsc file.
     *
     * @param avscPath   path to the .avsc file
     * @param schemaName unused, kept for API compatibility
     * @return the resolved Avro Schema
     * @throws IOException if the file cannot be read or parsed
     */
    public Schema load(String avscPath, String schemaName) throws IOException {
        String content = Files.readString(Path.of(avscPath)).trim();
        return new Schema.Parser().parse(content);
    }
}
