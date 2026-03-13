package example;

import example.cli.ConverterCli;

/**
 * JSON to Avro Schema Converter CLI Application.
 *
 * This application converts JSON files to Avro schema files (.avsc)
 * with automatic type inference.
 */
public class App {

    public static void main(String[] args) {
        ConverterCli cli = new ConverterCli();
        int exitCode = cli.run(args);
        System.exit(exitCode);
    }
}
