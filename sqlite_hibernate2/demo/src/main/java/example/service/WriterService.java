package example.service;

import java.util.Random;
import java.util.concurrent.ConcurrentMap;

public class WriterService extends Thread {
	private ConcurrentMap<Integer, String> data;
	private Random random;
	private String name;

	public WriterService(ConcurrentMap<Integer, String> data, String name,
			long randomSeed) {
		this.data = data;
		this.random = new Random(randomSeed);
		this.name = name;
	}

	public void run() {
		while (true) {
			Integer key = random.nextInt(10);
			String value = name;

			if (data.putIfAbsent(key, value) == null) {
				long time = System.currentTimeMillis();
				String output = String.format("%d: %s has put [%d => %s]", time, name,
						key, value);
				System.out.println(output);
			}

			Integer keyToRemove = random.nextInt(10);
			if (data.remove(keyToRemove, value)) {
				long time = System.currentTimeMillis();
				String output = String.format("%d: %s has removed [%d => %s]", time,
						name, keyToRemove, value);
				System.out.println(output);
			}

			try {
				Thread.sleep(500);
			} catch (InterruptedException ex) {
				ex.printStackTrace();
			}
		}
	}
}
