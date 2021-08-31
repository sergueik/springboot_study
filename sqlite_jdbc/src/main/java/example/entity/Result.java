package example.entity;

import java.io.Serializable;

@SuppressWarnings("serial")
public class Result implements Serializable {

	private int status;
	private Object data;

	public int getStatus() {
		return status;
	}

	public void setStatus(int status) {
		this.status = status;
	}

	public Object getData() {
		return data;
	}

	public void setData(Object data) {
		this.data = data;
	}
}
