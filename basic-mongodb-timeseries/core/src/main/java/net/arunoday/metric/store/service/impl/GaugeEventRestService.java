package net.arunoday.metric.store.service.impl;

import net.arunoday.metric.store.model.GaugeEvent;
import net.arunoday.metric.store.repository.GaugeEventRepository;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

/**
 * REST service for gauge events.
 * 
 * @author Aparna Chaudhary
 */
@RestController
@RequestMapping(value = "/events")
public class GaugeEventRestService {

	private static final Logger logger = LoggerFactory.getLogger(GaugeEventRestService.class);

	@Autowired
	private GaugeEventRepository<String> eventRepository;

	/**
	 * Stores gauge event
	 * 
	 * @param gaugeEvent gauge event to be stored
	 * @return identifier for the stored event
	 */
	@RequestMapping(value = "/", method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE)
	public String storeEvent(@RequestBody() GaugeEvent gaugeEvent) {
		logger.debug("storing gauge event: " + gaugeEvent);
		gaugeEvent = eventRepository.save(gaugeEvent);
		return gaugeEvent.getId();
	}

	/**
	 * Deletes all events of the provided event type
	 * 
	 * @param eventType event type for which events are to be deleted
	 */
	@RequestMapping(value = "/{eventType}", method = RequestMethod.DELETE, consumes = MediaType.APPLICATION_JSON_VALUE)
	public void deleteEvents(@PathVariable("eventType") String eventType) {
		logger.debug("deleting events of type: " + eventType);
		eventRepository.deleteAll(eventType);
	}

}
