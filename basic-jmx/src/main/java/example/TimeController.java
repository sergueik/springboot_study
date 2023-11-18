package example;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import java.util.Date;

@RestController
public class TimeController {

	private TimeService timeService;

	@Autowired
	public TimeController(TimeService timeService) {
		this.timeService = timeService;
	}

	@RequestMapping(value = "/timestamp", method = RequestMethod.GET)
	public String currentTimestamp() {
		return timeService.getCurrentTimestamp().orElse("TimeService disabled.");
	}

	@RequestMapping(value = "/count", method = RequestMethod.GET)
	public long count() {
		return timeService.getCount();
	}

}
