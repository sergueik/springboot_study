package com.github.yokra9.log4jna_sample;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class Logging {

    private static final Logger LOGGER = LogManager.getLogger(Logging.class);

    public void fatal(String message) {
        LOGGER.fatal(message);
    }

    public void error(String message) {
        LOGGER.error(message);
    }

    public void warn(String message) {
        LOGGER.warn(message);
    }

    public void info(String message) {
        LOGGER.info(message);
    }

    public void debug(String message) {
        LOGGER.debug(message);
    }

    public void trace(String message) {
        LOGGER.trace(message);
    }

}
