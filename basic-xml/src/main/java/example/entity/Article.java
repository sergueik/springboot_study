package example.entity;

public class Article {
	private String title;
	private String category;

	public Article(String title, String category) {
		this.title = title;
		this.category = category;
	}

	public String getTitle() {
		return title;
	}

	public String getCategory() {
		return category;
	}

	public String toString() {
		return "Article: " + category + " \"" + this.getTitle() + "\"";
	}
}