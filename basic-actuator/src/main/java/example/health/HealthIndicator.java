package example.health;

import org.springframework.boot.actuate.health.AbstractHealthIndicator;
import org.springframework.boot.actuate.health.Health;
import org.springframework.stereotype.Component;

@Component
public class HealthIndicator extends AbstractHealthIndicator {

	@Override
	protected void doHealthCheck(Health.Builder builder) throws Exception {
		// you throw an exception, then the status will becom DOWN
		builder.up().withDetail("app", "healthyy").withDetail("error", "no errors");
	}
}
