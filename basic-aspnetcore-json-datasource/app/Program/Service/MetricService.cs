using System.Threading;
using System.Threading.Tasks;
using System;

using Microsoft.Extensions.Hosting;

public class MetricService : BackgroundService {
	public MetricService() {
    buffer = new CircularBuffer<Data>(capacity);
}

		// 15 minute worth of data
		private CircularBuffer<Data> buffer;

		private static int capacity = 900;

    protected override async Task ExecuteAsync(
        CancellationToken stoppingToken)
    {
        using var timer = new PeriodicTimer(
            TimeSpan.FromSeconds(30));

        while (await timer.WaitForNextTickAsync(stoppingToken))
        {
            CollectMetrics();
        }
    }
    		private void CollectMetrics() {
			var value = DataGenerator.NextValue();
			var now = DateTime.UtcNow;
			var row = new Data();
			row.TimeStamp = now;
			row.Value = (float)value;
			buffer.AddLast(row);
			Console.WriteLine(String.Format("CollectMetrics: {0} {1}", now, value));
		}

}
