package example.application;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportResource;

// does not appear to work
@Configuration
@ImportResource({ "classpath*:applicationContext.xml" })
public class Movie {

	Actor actor;
	private String title;

	public String getTitle() {
		return title;
	}

	public void setTitle(String data) {
		this.title = data;
	}

	public Actor getActor() {
		return actor;
	}

	public void setActor(Actor actor) {
		this.actor = actor;
	}

	@Override
	public String toString() {
		return "Movie{" + "title=" + "\"" + getTitle() + "\", " + "actor=" + actor
				+ "}";
	}
}