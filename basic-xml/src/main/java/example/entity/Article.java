package example.entity;

public class Article {
	private int articleId;
	private String title;
	private String category;

	public Article(int articleId, String title, String category) {
		this.articleId = articleId;
		this.title = title;
		this.category = category;
	}

	public int getArticleId() {
		return articleId;
	}

	public String getTitle() {
		return title;
	}

	public String getCategory() {
		return category;
	}
}