package example;

import io.prometheus.client.CollectorRegistry;
import io.prometheus.client.Gauge;
import io.prometheus.client.exporter.PushGateway;

public class App {
	private int delay = 3000;

	void executeBatchJob(Boolean exception) throws Exception {
		CollectorRegistry registry = new CollectorRegistry();
		Gauge duration = Gauge.build().name("test_pushgateway_job_duration_seconds")
				.help("Duration of job in seconds.").register(registry);
		Gauge.Timer durationTimer = duration.startTimer();
		try {

			System.out.println("Executing job" + exception);
			Thread.sleep(delay);
			if (exception) {
				throw new Exception("Job exception");
			}

			System.out.println("Job Complete");
			// This is only added to the registry after success,
			// so that a previous success in the Pushgateway is not overwritten on
			// failure.
			Gauge lastSuccess = Gauge.build()
					.name("test_pushgateway_job_last_success_unixtime")
					.help("Last time my batch job succeeded, in unixtime.")
					.register(registry);
			lastSuccess.setToCurrentTime();
		} catch (Exception e) {
			System.out.println("Exception block");
			Gauge lastFailure = Gauge.build()
					.name("test_pushgateway_job_last_failure_unixtime")
					.help("Last time my batch job failed, in unixtime.")
					.register(registry);
			lastFailure.setToCurrentTime();

		} finally {
			durationTimer.setDuration();
			PushGateway pg = new PushGateway("127.0.0.1:9091");
			pg.pushAdd(registry, "test_pushgateway_job");
		}
	}

	public static void main(String[] args) {
		App app = new App();
		System.out.println("Starting Job");
		try {
			app.executeBatchJob(Boolean.valueOf(args[0]));
		} catch (Exception e) {
			System.out.println("Job failure");
		}

	}

}

