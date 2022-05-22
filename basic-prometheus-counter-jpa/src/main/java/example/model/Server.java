package example.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "server")
public class Server {

	private int serverId;
	private String serverName;

	public Server() {
	}

	public Server(String serverName) {
		this.serverName = serverName;
	}

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	@Column(name = "sid")
	public int getServerId() {
		return serverId;
	}

	public void setServerId(int value) {
		serverId = value;
	}

	@Column(name = "sname", nullable = false, length = 10)
	public String getServerName() {
		return serverName;
	}

	public void setServerName(String value) {
		serverName = value;
	}

}
