package example.service;

import java.util.ArrayList;
import java.util.List;

import example.entity.Article;

public class ArticleService {
	private MessageService messageService;
	private Article article;

	public Article getArticle() {
		return article;
	}

	public void setArticle(Article data) {
		this.article = data;
	}

	public void setMessageService(MessageService data) {
		this.messageService = data;
	}

	public String processMsg() {
		List<Object> data = new ArrayList<>();
		data.add((Object) article);
		return messageService.sendData(data);
	}
}
