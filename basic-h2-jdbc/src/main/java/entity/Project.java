package entity;

public class Project {

	private Long id;
	private String title;

	public Project() {

	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	@Override
	public boolean equals(Object o) {
		if (this == o)
			return true;
		if (o == null || getClass() != o.getClass())
			return false;

		Project project = (Project) o;

		if (id != null ? !id.equals(project.id) : project.id != null)
			return false;
		return title != null ? title.equals(project.title) : project.title == null;
	}

	@Override
	public int hashCode() {
		int result = id != null ? id.hashCode() : 0;
		result = 31 * result + (title != null ? title.hashCode() : 0);
		return result;
	}

	@Override
	public String toString() {
		return "Project{" + "id=" + id + ", title='" + title + '\'' + '}';
	}
}
