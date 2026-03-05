package example;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

@Component
@ConfigurationProperties(prefix = "example")
public class ExampleProperties {

	private String baseBadPath;
	private String baseGoodPath;
	
	// Spring populates Lists automatically, no SPL with split hacks needed
	private List<String> goodCopybooks;

	private List<String> badCopybooks;

	// The Spring @ConfigurationProperties libraries support Map type natively - no
	// inline "{}" in the property file meeded
	private Map<String, String> metricExtractors;

	public List<String> getGoodCopybooks() {
		return goodCopybooks;
	}

	public void setGoodCopybooks(List<String> goodCopybooks) {
		this.goodCopybooks = goodCopybooks;
	}

	public List<String> getBadCopybooks() {
		return badCopybooks;
	}

	public void setBadCopybooks(List<String> badCopybooks) {
		this.badCopybooks = badCopybooks;
	}

	public Map<String, String> getMetricExtractors() {
		return metricExtractors;
	}

	public void setMetricExtractors(Map<String, String> metricExtractors) {
		this.metricExtractors = metricExtractors;
	}

	public String getBaseBadPath() {
		return baseBadPath;
	}

	public void setBaseBadPath(String baseBadPath) {
		this.baseBadPath = baseBadPath;
	}

	public String getBaseGoodPath() {
		return baseGoodPath;
	}

	public void setBaseGoodPath(String baseGoodPath) {
		this.baseGoodPath = baseGoodPath;
	}

}