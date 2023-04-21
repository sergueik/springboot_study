package example.model;

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

@Document
public class Ticket {

	@Id
	private String id;
	private String appId;
	private String status;

	public Ticket() {
	}

	// TODO: constructor with id
	
	public Ticket(String appId, String status) {
		this.appId = appId;
		this.status = status;
	}

	public Ticket(String status) {
		this.status = status;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getAppId() {
		return appId;
	}

	public void setAppId(String appId) {
		this.appId = appId;
	}

	@Override
	public String toString() {
		return "Ticket{" + "id=" + id + ", application_id=" + appId + ", status='"
				+ status + '\'' + '}';
	}
}
