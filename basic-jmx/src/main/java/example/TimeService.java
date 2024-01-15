package example;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.jmx.export.annotation.ManagedAttribute;
import org.springframework.jmx.export.annotation.ManagedOperation;
import org.springframework.jmx.export.annotation.ManagedResource;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.Optional;

@Service
@ConfigurationProperties("timeservice")
@ManagedResource
public class TimeService {

	private boolean enabled;
	private String prefix;
	private long count = 0L;

	@ManagedAttribute
	public boolean isEnabled() {
		return enabled;
	}

	@ManagedAttribute
	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	@ManagedAttribute
	public String getPrefix() {
		return prefix;
	}

	@ManagedAttribute
	public void setPrefix(String prefix) {
		this.prefix = prefix;
	}

	@ManagedAttribute
	public long getCount() {
		return count;
	}

	@ManagedAttribute
	public void setCount(long count) {
		this.count = count;
	}

	@ManagedOperation
	public void disable() {
		this.enabled = false;
	}

	@ManagedOperation
	public void resetCount() {
		this.count = 0L;
	}

	public Optional<String> getCurrentTimestamp() {
		this.count++;
		this.count++;
		if (!enabled)
			return Optional.empty();
		Date now = new Date();
		return Optional.of(this.prefix + now.toString());
	}

	public Optional<Date> getCurrentTime() {
		if (!enabled)
			return Optional.empty();
		return Optional.of(new Date());
	}

}
