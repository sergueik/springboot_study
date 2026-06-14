package example;

import me.desair.tus.server.TusFileUploadService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ApplicationListener;
import org.springframework.context.annotation.Bean;
import org.springframework.context.event.ContextRefreshedEvent;

@SpringBootApplication
public class Application implements ApplicationListener<ContextRefreshedEvent> {

	private static final Logger logger = LoggerFactory.getLogger(Application.class);

	@Value("${spring.profiles.active}")
	protected String springProfilesActive;

	@Value("${tus.server.data.directory}")
	protected String tusDataPath;

	@Value("#{servletContext.contextPath}")
	private String servletContextPath;

	@Override
	public void onApplicationEvent(ContextRefreshedEvent event) {
		logger.info("App running with active profiles: {}", springProfilesActive);
	}

	public static void main(String[] args) throws Exception {
		SpringApplication.run(Application.class, args);
	}

	@Bean
	public TusFileUploadService tusFileUploadService() {
		// https://javadoc.io/doc/me.desair.tus/tus-java-server/latest/index.html
		return new TusFileUploadService().withStoragePath(tusDataPath).withDownloadFeature()
				.withUploadUri(servletContextPath + "/api/upload").withThreadLocalCache(true);
	}

}
