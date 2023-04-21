package net.arunoday.metric.store.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.argThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;
import net.arunoday.metric.store.model.ContextData;
import net.arunoday.metric.store.model.GaugeEvent;
import net.arunoday.metric.store.repository.GaugeEventRepository;
import net.arunoday.metric.store.service.impl.GaugeEventRestService;

import org.hamcrest.Matchers;
import org.joda.time.DateTime;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.web.servlet.MockMvc;

/**
 * Tests for {@link GaugeEventRestService}
 * 
 * @author Aparna Chaudhary
 */
public class GaugeEventRestServiceIT {

	@InjectMocks
	private GaugeEventRestService eventRestService;

	@Mock
	GaugeEventRepository<String> eventRepository;

	protected MockMvc mockMvc;

	@Before
	public void setup() throws Exception {
		MockitoAnnotations.initMocks(this);
		this.mockMvc = standaloneSetup(eventRestService).setMessageConverters(new MappingJackson2HttpMessageConverter()).build();
	}

	@Test
	public void test_invalidURL() throws Exception {
		mockMvc.perform(get("/event/minute/request/2013")).andExpect(status().isNotFound());
	}

	@Test
	public void testSave() throws Exception {
		DateTime occuredOn = new DateTime("2013-08-10T16:00:00.389Z");
		GaugeEvent event = new GaugeEvent(occuredOn.toDate(), "restTest", 11.0);
		ContextData data = new ContextData();
		data.put("hostname", "localhost");
		event.setContextData(data);
		event.setId("uniqueEventId");

		when(eventRepository.save(any(GaugeEvent.class))).thenReturn(event);

		String eventJson = "{\"id\":\"uniqueEventId\", \"occuredOn\":\"2013-08-10T16:00:00.389Z\","
				+ "\"eventType\":\"restTest\",\"value\":11.0,\"contextData\":{\"hostname\":\"localhost\"}}";

		mockMvc.perform(post("/events/").content(eventJson).contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk()).andExpect(content().string("\"uniqueEventId\""));

		verify(eventRepository).save(argThat(Matchers.<GaugeEvent> hasProperty("eventType", Matchers.equalTo("restTest"))));
	}

	@Test
	public void testDelete() throws Exception {
		mockMvc.perform(delete("/events/{eventId}", "restTest").contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)).andExpect(
				status().isOk());

		verify(eventRepository).deleteAll(eq("restTest"));
	}

}
