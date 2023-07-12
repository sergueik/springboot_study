package example;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;


public class Example {
private static final Logger logger = LogManager.getLogger(Example.class);

    public static void main(String[] args) {
        logger.debug("Debug log message");
        logger.info("Info log message");
        logger.error("Error log message");
        logger.warn("Warn log message");
        logger.fatal("Fatal log message");
        logger.trace("Trace log message");
    }
}
