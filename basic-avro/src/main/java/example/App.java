package example;

import example.cli.ConverterCli;

public class App {

    public static void main(String[] args) {
        ConverterCli cli = new ConverterCli();
        int exitCode = cli.run(args);
        System.exit(exitCode);
    }
}
