package example.health;

import org.springframework.boot.actuate.health.AbstractHealthIndicator;
import org.springframework.boot.actuate.health.Health;
import org.springframework.stereotype.Component;

@Component
public class HealthIndicator extends AbstractHealthIndicator {

	@Override
	protected void doHealthCheck(Health.Builder builder) throws Exception {
		// when  an exception is thrown, then the status will become DOWN
		builder.up().withDetail("app", "healthyy").withDetail("error", "no errors");
	}
}
