package example.controller;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import example.entity.Article;
import example.service.ArticleService;

@RestController
@RequestMapping("user")
public class ArticleController {
	private ArticleService articleService;

	public void setArticleService(ArticleService data) {
		this.articleService = data;
	}

	/*
	 * NOTE: failing in runtime: Error creating bean with name
	 * 'org.springframework.web.servlet.mvc.method.annotation.
	 * RequestMappingHandlerMapping': Invocation of init method failed; nested
	 * exception is java.lang.IllegalStateException: Ambiguous mapping. Cannot map
	 * 'example.controller.ArticleController#0' method public
	 * java.util.List<example.entity.Article>
	 * example.controller.ArticleController.getAllArticles() to
	 * {[/user/articles],methods=[GET]}: There is already 'articleController' bean
	 * method public java.util.List<example.entity.Article>
	 * example.controller.ArticleController.getAllArticles() mapped.
	 * 
	 */
	@GetMapping("articles")
	public List<Article> getAllArticles() {
		List<Article> list = new ArrayList<>();
		list.add(articleService.getArticle());
		return list;
	}
}