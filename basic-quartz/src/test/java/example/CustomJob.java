package example;

import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import example.service.SampleService;

public class CustomJob implements Job {

	private Utils utils = Utils.getInstance();
	private static final Logger LOG = LoggerFactory.getLogger(CustomJob.class);

	@Override
	public void execute(JobExecutionContext jobExecutionContext) {
		utils.setDebug(true);
		utils.setInfo("information");
		LOG.info("setting information: " + utils.getInfo());
	}
}
