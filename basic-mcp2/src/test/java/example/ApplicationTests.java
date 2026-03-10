package example;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.context.ActiveProfiles;
import example.tools.youtube.YouTubeService;
import example.tools.blog.BlogService;
import example.tools.speaking.SpeakingService;
import example.tools.newsletter.NewsletterService;
import example.tools.podcast.PodcastService;

@SpringBootTest
@ActiveProfiles("test")
class ApplicationTests {

	@MockitoBean
	private YouTubeService youTubeService;

	@MockitoBean
	private BlogService blogService;

	@MockitoBean
	private SpeakingService speakingService;

	@MockitoBean
	private NewsletterService newsletterService;

	@MockitoBean
	private PodcastService podcastService;

	@Test
	void contextLoads() {
		// Context loading test - all services are mocked to prevent API calls
	}

}
