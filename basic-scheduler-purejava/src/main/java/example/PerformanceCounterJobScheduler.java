package example;

public class PerformanceCounterJobScheduler {

	private final static int interval = 1000;

	public static void main(String[] args) throws Exception {

		Runnable collectorTask = new PerformanceCounterTask();
		PerformanceCounterTask computeTask = new PerformanceCounterTask();
		computeTask.setTask(MessageType.COMPUTE);
		JobScheduler jobScheduler = new JobScheduler(0);
		jobScheduler.executeInAndRepeat(collectorTask, interval,
				JobScheduler.PER_SECOND);
		jobScheduler.executeInAndRepeat(computeTask, interval * 10,
				30 * JobScheduler.PER_SECOND);
	}

}
