package example;

public class PerformanceCounterJobRunner {

	private final static int interval = 1000;

	public static void main(String[] args) throws Exception {
		Runnable performanceCounterTask = new PerformanceCounterTask();

		for (int cnt = 0; cnt != 10; cnt++) {
			Thread.sleep(interval);
			performanceCounterTask.run();
		}
	}

}
