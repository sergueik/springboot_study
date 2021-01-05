package example;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.concurrent.ThreadLocalRandom;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.boot.actuate.health.Health;
import org.springframework.boot.actuate.health.HealthContributor;
import org.springframework.boot.actuate.health.HealthIndicator;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.RestController;

@RestController
@Component("random")
public class HeartbeatControllerExample implements HealthIndicator, HealthContributor {

	private static final Logger logger = LogManager.getLogger(HeartbeatControllerExample.class);

	public static Logger getLogger() {
		return logger;
	}

	@Override
	public Health health() {

		double value = ThreadLocalRandom.current().nextDouble();
		Health.Builder status = (value > 0.9)
				? Health.down().withDetail("error", "" /* the value apears to be overwrritten */)
						.withDetail("message", String.format("value: %04.2f", value))
						.withDetail("timestamp", getCurrentDateTimeStamp()).withException(
								new RuntimeException("exception details"))
				: Health.up();
		logger.info(status.build());
		return status.build();
	}

	private String getCurrentDateTimeStamp() {
		return (new SimpleDateFormat("MMM dd,yyyy HH:mm")).format(new Date(System.currentTimeMillis()));
	}
}
