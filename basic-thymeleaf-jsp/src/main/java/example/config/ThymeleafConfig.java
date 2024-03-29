package example.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.ViewResolver;
import org.thymeleaf.ITemplateEngine;
import org.thymeleaf.spring4.SpringTemplateEngine;
import org.thymeleaf.spring4.templateresolver.SpringResourceTemplateResolver;
import org.thymeleaf.spring4.view.ThymeleafViewResolver;
import org.thymeleaf.templatemode.TemplateMode;
import org.thymeleaf.templateresolver.ITemplateResolver;

@Configuration
public class ThymeleafConfig {

	@Autowired
	private ApplicationContext applicationContext;

	// NOTE: can not have identically named @Bean annotated methods across
	// classes:
	// compile time error:
	// The bean 'viewResolver', defined in class path resource
	// [example/config/ThymeleafConfig.class], could not be registered. A bean
	// with that name has already been defined in class path resource
	// [example/config/JspConfig.class] and overriding is disabled.
	@Bean
	public ViewResolver thymeleafConfigViewResolver(
			ITemplateEngine templateEngine) {
		ThymeleafViewResolver viewResolver = new ThymeleafViewResolver();

		viewResolver.setOrder(1);
		viewResolver.setTemplateEngine(templateEngine);
		viewResolver.setCharacterEncoding("UTF-8");
		viewResolver.setViewNames(new String[] { "*.html" });

		return viewResolver;
	}

	@Bean
	public ITemplateEngine templateEngine(ITemplateResolver templateResolver) {
		SpringTemplateEngine templateEngine = new SpringTemplateEngine();

		templateEngine.setTemplateResolver(templateResolver);
		templateEngine.setEnableSpringELCompiler(true);

		return templateEngine;
	}

	@Bean
	public ITemplateResolver templateResolver() {
		SpringResourceTemplateResolver templateResolver = new SpringResourceTemplateResolver();

		templateResolver.setApplicationContext(this.applicationContext);
		templateResolver.setTemplateMode(TemplateMode.HTML);
		templateResolver.setPrefix("/WEB-INF/templates");
		templateResolver.setCharacterEncoding("UTF-8");
		templateResolver.setCacheable(false);

		return templateResolver;
	}
}
