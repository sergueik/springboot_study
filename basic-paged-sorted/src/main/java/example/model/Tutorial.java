package example.model;

import javax.persistence.*;

@Entity
@Table(name = "tutorials")
public class Tutorial {

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;

	@Column(name = "title")
	private String title;

	@Column(name = "description")
	private String description;

	@Column(name = "published")
	private boolean published;

	public Tutorial() {

	}

	public Tutorial(String title, String description, boolean published) {
		this.title = title;
		this.description = description;
		this.published = published;
	}

	public long getId() {
		return id;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String data) {
		title = data;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String data) {
		description = data;
	}

	public boolean isPublished() {
		return published;
	}

	public void setPublished(boolean data) {
		published = data;
	}

	@Override
	public String toString() {
		return "{\"" + this.getClass().getSimpleName() + "\": {\"id\":\"" + id
				+ "\", \"title\":\"" + title + "\", \"desc\":\"" + description
				+ "\", \"published\":\"" + published + "\"}";
	}

}
