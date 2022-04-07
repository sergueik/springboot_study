package example;

public class PerformanceCounterJobScheduler {

	private final static int interval = 1000;

	public static void main(String[] args) throws Exception {

		Runnable performanceCounterTask = new PerformanceCounterTask();

		JobScheduler jobScheduler = new JobScheduler(0);
		jobScheduler.executeInAndRepeat(performanceCounterTask, interval,
				JobScheduler.PER_SECOND);
	}

}
