package example.controller;

import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobInstance;
import org.springframework.batch.core.JobParameter;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersInvalidException;
// import org.springframework.batch.core.*;
import org.springframework.batch.core.explore.JobExplorer;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.batch.core.launch.NoSuchJobException;
import org.springframework.batch.core.repository.JobExecutionAlreadyRunningException;
import org.springframework.batch.core.repository.JobInstanceAlreadyCompleteException;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.repository.JobRestartException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@RestController
@RequestMapping("/")
public class LoadController {

	private static final Logger logger = LogManager.getLogger(LoadController.class);

	private final String jobName = "ETL-load";

	@Autowired
	JobExplorer jobExplorer;

	@Autowired
	JobLauncher jobLauncher;

	@Autowired
	Job job;

	@Autowired
	JobRepository jobRepository;

	@GetMapping("status")
	public BatchStatus status() throws JobParametersInvalidException, JobExecutionAlreadyRunningException,
			JobRestartException, JobInstanceAlreadyCompleteException, NoSuchJobException {
		BatchStatus jobStatus;
		logger.info("Job names: " + jobExplorer.getJobNames());
		List<JobInstance> jobInstances = jobExplorer.getJobInstances(jobName, 0, 10);
		int cnt = jobExplorer.getJobInstanceCount(jobName);

		cnt = jobInstances.size();
		logger.info(String.format("Job %s instances count: %d", jobName, cnt));
		if (cnt != 0) {
			logger.info(String.format("Job %s executions count: %d", jobName,
					jobExplorer.getJobExecutions(jobInstances.get(0)).size()));
			JobExecution jobExecution = jobExplorer.getJobExecutions(jobInstances.get(0)).get(0);
			jobStatus = jobExecution.getStatus();
		} else {
			logger.info(String.format("Checking Job %s repository execution", jobName));
			JobExecution repositoryJobExecution = jobRepository.getLastJobExecution("ETL-file-load",
					new JobParameters());
			if (repositoryJobExecution != null) {
				jobStatus = repositoryJobExecution.getStatus();
			} else {
				jobStatus = BatchStatus.UNKNOWN;
			}
		}
		logger.info(String.format("Job %s status is %s", jobName, jobStatus.toString()));

		return jobStatus;
	}

	@GetMapping("load")
	public BatchStatus load() throws JobParametersInvalidException, JobExecutionAlreadyRunningException,
			JobRestartException, JobInstanceAlreadyCompleteException, NoSuchJobException {

		Map<String, JobParameter> maps = new HashMap<>();
		maps.put("time", new JobParameter(System.currentTimeMillis()));
		JobParameters parameters = new JobParameters(maps);
		// do we need to match parameters ?
		// parameters = new JobParameters();
		JobExecution jobExecution = jobLauncher.run(job, parameters);
		logger.info(String.format("Job name: %s", jobName));
		JobInstance jobInstance = jobExplorer.getLastJobInstance(jobName);
		if (jobInstance != null) {
			logger.info(String.format("Last Job Instance: %s", jobInstance));
			logger.info(String.format("Job instance id: %d", jobName, jobInstance.getInstanceId()));
		} else
			logger.info("Last Job Instance is null");
		if (jobExecution == null) {
			logger.info("JobExecution is null");
		} else {
			logger.info("JobExecution: " + jobExecution.getStatus());
		}
		logger.info(String.format("Batch %s is launched", job.getName()));
		/*
		 * while (jobExecution.isRunning()) { logger.info("..."); }
		 */
		return jobExecution.getStatus();
	}
}
