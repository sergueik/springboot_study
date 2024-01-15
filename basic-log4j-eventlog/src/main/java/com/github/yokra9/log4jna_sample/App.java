package com.github.yokra9.log4jna_sample;

public class App {

    /**
     * Demo to use log4jna
     * 
     * @param args message to log
     */
    public static void main(String[] args) {

        StringBuilder b = new StringBuilder();
        for (String str : args) {
            b.append(str);
            b.append(' ');
        }
        String message = b.toString();

        Logging l = new Logging();

        l.fatal(message);
        l.error(message);
        l.warn(message);
        l.info(message);
        l.debug(message);
        l.trace(message);

    }
}
