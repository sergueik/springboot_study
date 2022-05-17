package example.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "axixs")
public class Axixs {

	// The identity coumn is defined but the database shema appears unchanged
	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	@Column(name = "xid")
	private int axixsId;

	// NOTE: without primary key, get error in startup
	// Caused by: org.springframework.beans.factory.BeanCreationException:
	// Error creating bean with name 'jpaMappingContext':
	// Invocation of init method failed; nested exception is
	// org.hibernate.AnnotationException: No identifier specified for entity:
	// example.model.Axixs

	@Column(name = "iid")
	private int instanceId;
	@Column(name = "aid")
	private int applicationId;
	@Column(name = "sid")
	private int serverId;

	public Axixs() {
	}

	public Axixs(int instanceId, int applicationId, int serverId) {
		this.instanceId = instanceId;
	}

	public int getInstanceId() {
		return instanceId;
	}

	public void setInstanceId(int value) {
		instanceId = value;
	}

	public int getApplicationId() {
		return applicationId;
	}

	public void setApplicationId(int value) {
		applicationId = value;
	}

	public int getServerId() {
		return serverId;
	}

	public void setServerId(int value) {
		serverId = value;
	}
}
