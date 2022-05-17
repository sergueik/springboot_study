package example.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "instance")
public class Instance {

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	@Column(name = "iid")
	private int instanceId;

	@Column(name = "iname", nullable = false, length = 10)
	private String instanceName;

	public Instance() {
	}

	public Instance(String instanceName) {
		this.instanceName = instanceName;
	}

	public int getInstanceId() {
		return instanceId;
	}

	public void setInstanceId(int value) {
		instanceId = value;
	}

	public String getInstanceName() {
		return instanceName;
	}

	public void setInstanceName(String value) {
		instanceName = value;
	}

}
