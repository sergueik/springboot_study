package example;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConfigurationProperties(prefix = "application")
// @ConstructorBinding 2.2.2
public class ApplicationProperties {

	// @Value(value = "")
	private String artistName;

	public String getArtistName() {
		return this.artistName;
	}

	public void setArtistName(String value) {
		artistName = value;
	}

}
