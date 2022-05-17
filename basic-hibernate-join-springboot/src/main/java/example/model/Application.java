package example.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "application")
public class Application {

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	@Column(name = "aid")
	private int applicationId;

	@Column(name = "aname", nullable = false, length = 10)
	private String applicationName;

	public Application() {
	}

	public Application(String applicationName) {
		this.applicationName = applicationName;
	}

	public int getApplicationId() {
		return applicationId;
	}

	public void setApplicationId(int value) {
		applicationId = value;
	}

	public String getApplicationName() {
		return applicationName;
	}

	public void setApplicationName(String value) {
		applicationName = value;
	}

}
