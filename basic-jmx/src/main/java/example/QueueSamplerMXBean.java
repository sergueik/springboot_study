package example;

import java.util.Queue;

public interface QueueSamplerMXBean {
	public QueueSample getQueueSample();

	public void clearQueue();
}
