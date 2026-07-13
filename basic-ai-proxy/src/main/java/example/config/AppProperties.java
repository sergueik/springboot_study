package example.config;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Component
@ConfigurationProperties(prefix = "app")
public class AppProperties {
	private Security security = new Security();
	private String providersJson = "{}";
	private Map<String, ProviderProperties> providers = new LinkedHashMap<>();

	public Security getSecurity() {
		return security;
	}

	public void setSecurity(Security security) {
		this.security = security;
	}

	public String getProvidersJson() {
		return providersJson;
	}

	public void setProvidersJson(String providersJson) {
		this.providersJson = providersJson;
	}

	public Map<String, ProviderProperties> getProviders() {
		return providers;
	}

	public void setProviders(Map<String, ProviderProperties> providers) {
		this.providers = providers;
	}

	public static class Security {
		private boolean enabled = true;
		private List<String> allowedIps = new ArrayList<>();
		private String adminToken;
		private long dynamicIpTtlMinutes = 720;

		public boolean isEnabled() {
			return enabled;
		}

		public void setEnabled(boolean enabled) {
			this.enabled = enabled;
		}

		public List<String> getAllowedIps() {
			return allowedIps;
		}

		public void setAllowedIps(List<String> allowedIps) {
			this.allowedIps = allowedIps;
		}

		public String getAdminToken() {
			return adminToken;
		}

		public void setAdminToken(String adminToken) {
			this.adminToken = adminToken;
		}

		public long getDynamicIpTtlMinutes() {
			return dynamicIpTtlMinutes;
		}

		public void setDynamicIpTtlMinutes(long dynamicIpTtlMinutes) {
			this.dynamicIpTtlMinutes = dynamicIpTtlMinutes;
		}
	}
}