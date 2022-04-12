package example.custom;

import example.JobScheduler;

public class PerformanceCounterJobScheduler {

	private final static int interval = 1000;

	public static void main(String[] args) throws Exception {

		PerformanceCounterTask collectorTask = new PerformanceCounterTask();
		collectorTask.setVerbose(true);
		PerformanceCounterTask computeTask = new PerformanceCounterTask();
		computeTask.setVerbose(true);
		computeTask.setTask(MessageType.COMPUTE);
		JobScheduler jobScheduler = new JobScheduler(0);
		jobScheduler.executeInAndRepeat(collectorTask, interval,
				JobScheduler.PER_SECOND);
		jobScheduler.executeInAndRepeat(computeTask, interval * 10,
				30 * JobScheduler.PER_SECOND);
	}

}
