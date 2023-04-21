package net.arunoday.metric.store.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.sql.Date;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import net.arunoday.metric.store.model.HierarchialAggregationResult;
import net.arunoday.metric.store.model.MetricResolution;
import net.arunoday.metric.store.repository.GaugeMetricRepository;
import net.arunoday.metric.store.service.impl.GaugeMetricRestService;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.web.servlet.MockMvc;

/**
 * Tests for {@link GaugeMetricRestService}
 * 
 * @author Aparna Chaudhary
 */
public class GaugeMetricRestServiceIT {

	@InjectMocks
	private GaugeMetricRestService metricRestService;

	@Mock
	GaugeMetricRepository<String> metricRepository;

	protected MockMvc mockMvc;

	@Before
	public void setup() throws Exception {
		MockitoAnnotations.initMocks(this);
		this.mockMvc = standaloneSetup(metricRestService).setMessageConverters(new MappingJackson2HttpMessageConverter())
				.build();
	}

	@Test
	public void test_invalidURL() throws Exception {
		mockMvc.perform(get("/metric/minute/request/2013")).andExpect(status().isNotFound());
		mockMvc.perform(get("/metric/hourly/request/2013")).andExpect(status().isNotFound());
		mockMvc.perform(get("/metric/daily/request/")).andExpect(status().isNotFound());
		mockMvc.perform(get("/metric/monthly/request/")).andExpect(status().isNotFound());
		mockMvc.perform(get("/metric/yearly/request/")).andExpect(status().isNotFound());
	}

	@Test
	public void testMinuteAggregation_PerMonth() throws Exception {
		String eventType = "restTest";
		Map<String, Object> value = new HashMap<String, Object>();
		value.put("total", 10);
		value.put("count", 56);
		value.put("min", 1);
		HierarchialAggregationResult data = new HierarchialAggregationResult();
		data.setId("2013-12-10T16:00:00.000Z");
		data.setValue(value);

		when(metricRepository.find(eq(eventType), eq(MetricResolution.MINUTE), any(Date.class), any(Date.class)))
				.thenReturn(Arrays.asList(data));

		mockMvc.perform(get("/metric/minute/".concat(eventType).concat("/2013/12/10"))).andExpect(status().isOk())
				.andExpect(jsonPath("$[0].value.min").value(1)).andExpect(status().isOk())
				.andExpect(jsonPath("$[0].value.count").value(56));
	}
}
