package example.entity;

import java.io.Serializable;
import java.sql.Timestamp;

@SuppressWarnings("serial")
public class Student implements Serializable {

	private long id;
	private String name;
	private String course;
	// private Timestamp addtime;

	public long getId() {
		return id;
	}

	public void setId(long value) {
		id = value;
	}

	public String getName() {
		return name;
	}

	public void setName(String value) {
		name = value;
	}

	public String getCourse() {
		return course;
	}

	public void setCourse(String value) {
		course = value;
	}

	/*
	 * public Timestamp getAddtime() { return addtime; } public void
	 * setAddtime(Timestamp addtime) { this.addtime = addtime; }
	 */
	@Override
	public String toString() {
		return "Student{" + "id=" + this.id + ", name=" + this.name + ", course="
				+ this.course
				/* + ", addtime=" + addtime */ + '}';
	}
}
