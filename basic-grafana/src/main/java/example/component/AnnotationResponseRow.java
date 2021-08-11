package example.component;

public class AnnotationResponseRow {
	private Annotation annotation;

	public Annotation getAnnotation() {
		return annotation;
	}

	public void setAnnotation(Annotation annotation) {
		this.annotation = annotation;
	}

	public String getTime() {
		return time;
	}

	public void setTime(String time) {
		this.time = time;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getText() {
		return text;
	}

	public void setText(String text) {
		this.text = text;
	}

	private String time; // todo: stringly typeas Unix Time
	private String title; // The title for the annotation tooltip. (required)
	// private tags: tags; // Tags for the annotation. (optional)
	private String text;
}
