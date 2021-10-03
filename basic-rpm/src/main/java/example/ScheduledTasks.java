package example;

import java.text.SimpleDateFormat;
import java.util.Date;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.Level;

@Configuration
@PropertySource("classpath:application.properties")
@Component
public class ScheduledTasks {

	@Value("${logfile}")
	private String logfile;
	private Tailer tailer;
	private static final Logger logger = LogManager
			.getLogger(ScheduledTasks.class);

	private static final SimpleDateFormat dateFormat = new SimpleDateFormat(
			"MM/dd/yyyy HH:mm:ss");

	// NOTE: cannot use /* */ comments acount the annotated method due to conflict
	// with cron notation

	// ┌───────────── minute (0 - 59)
	// │ ┌───────────── hour (0 - 23)
	// │ │ ┌───────────── day of the month (1 - 31)
	// │ │ │ ┌───────────── month (1 - 12)
	// │ │ │ │ ┌───────────── day of the week (0 - 6) (Sunday to Saturday;
	// │ │ │ │ │ 7 is also Sunday on some systems)
	// │ │ │ │ │
	// │ │ │ │ │
	// * * * * * <command to execute>
	// It appears the cron task is fired too often.
	// As if the leading column is seconds, not minites
	// the classic cron has 5 fields
	// Spring has 6 fields
	// @Scheduled(cron = "0/2 * * * * *")

	private int length = 0;

	@Scheduled(cron = "0 */2 * * * *")
	public void cronTask() {
		logger.info("Cron task performed at " + dateFormat.format(new Date())
				+ " on thread " + Thread.currentThread().getName());
		if (tailer == null) {
			tailer = new Tailer();
			tailer.setFilePath(logfile);
			tailer.setOffset(0);
		} else {
			tailer.setOffset(tailer.getLength());
		}
		tailer.tail();
		System.out.println(
				String.format("rpm: %d /%d ", tailer.rpm(), tailer.rpm(false)));
	}

	@Scheduled(fixedRate = 120000)
	public void fixedRateTask() {
		logger.info("Fixed rate task performed at " + dateFormat.format(new Date())
				+ " on thread " + Thread.currentThread().getName());
	}

}
