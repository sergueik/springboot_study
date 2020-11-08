package example;
import org.apache.log4j.Logger;
 
public class Basic {
    private static final Logger LOGGER = Logger.getLogger(Basic.class);
    public static void main(String a[]) {
        LOGGER.debug("Debug message");
        LOGGER.info("Info message" );
    }
}
