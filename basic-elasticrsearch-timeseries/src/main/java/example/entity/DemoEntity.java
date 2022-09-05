package example.entity;

import java.util.Date;

public class DemoEntity {
	private String id;
	private String name;
	private Date createTime;

	public String getId() {
		return id;
	}

	public void setId(String value) {
		id = value;
	}

	public String getName() {
		return name;
	}

	public void setName(String value) {
		name = value;
	}

	public Date getCreateTime() {
		return createTime;
	}

	public void setCreateTime(Date value) {
		createTime = value;
	}
}
