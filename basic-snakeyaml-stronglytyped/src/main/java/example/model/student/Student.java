package example.model.student;

import java.util.List;
import java.util.StringJoiner;

public class Student extends Person {

	private int year;
	private String department;
	private List<Course> courses;

	public int getYear() {
		return year;
	}

	public void setYear(int year) {
		this.year = year;
	}

	public String getDepartment() {
		return department;
	}

	public void setDepartment(String department) {
		this.department = department;
	}

	public List<Course> getCourses() {
		return courses;
	}

	public void setCourses(List<Course> courses) {
		this.courses = courses;
	}

	@Override
	public String toString() {
		return new StringJoiner(", ", Student.class.getSimpleName() + "[", "]")
				.add(super.toString()).add("year=" + year)
				.add("department='" + department + "'").add("courses=" + courses)
				.toString();
	}
}
