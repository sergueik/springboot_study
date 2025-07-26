package example;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
public class WebConfig implements WebMvcConfigurer {

    @Override
    public void addCorsMappings(CorsRegistry registry) {
        // Configure CORS settings
        registry.addMapping("/api/**")
                .allowedOrigins("http://localhost:3000") // Allow requests from this origin
                .allowedMethods("GET", "POST", "PUT", "DELETE") // Allowed HTTP methods
                .allowCredentials(true); // Allow sending credentials (cookies, HTTP authentication)
    }

    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        // Configure resource handlers for static content
        registry.addResourceHandler("/static/**")
                .addResourceLocations("classpath:/static/"); // Map /static to classpath:/static/
    }

    // You can override other methods from WebMvcConfigurer to customize:
    // - configurePathMatch: To customize URL path matching (e.g., trailing slash)
    // - addViewControllers: To register simple automated controllers for view names
    // - configureContentNegotiation: To customize content negotiation
    // - addInterceptors: To add custom interceptors
}