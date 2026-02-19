package example.listener;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobExecutionListener;
import org.springframework.batch.core.BatchStatus;
import org.springframework.stereotype.Component;

@Component
public class Listener implements JobExecutionListener {

    private static final Logger logger = LoggerFactory.getLogger(Listener.class);

    @Override
    public void beforeJob(JobExecution jobExecution) {
        logger.info("Job is starting. Job name: {}", jobExecution.getJobInstance().getJobName());
        // You can add any pre-job logic here
    }

    @Override
    public void afterJob(JobExecution jobExecution) {
        logger.info("Job finished. Job name: {}", jobExecution.getJobInstance().getJobName());
        logger.info("Status: {}", jobExecution.getStatus());

        if (jobExecution.getStatus() == BatchStatus.COMPLETED) {
            logger.info("Job completed successfully!");
        } else if (jobExecution.getStatus() == BatchStatus.FAILED) {
            logger.error("Job failed with exceptions:");
            jobExecution.getAllFailureExceptions()
                        .forEach(ex -> logger.error(" - {}", ex.getMessage(), ex));
        }
    }
}
