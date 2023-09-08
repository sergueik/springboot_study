package example.service;

import java.util.Iterator;
import java.util.concurrent.ConcurrentHashMap;

import example.component.DataComponent;

public class ReaderService extends Thread {
	private DataComponent data;
	private String name;

	public ReaderService(DataComponent data, String name) {
		this.data = data;
		this.name = name;
	}

	public void run() {
		while (true) {
			ConcurrentHashMap.KeySetView<Integer, String> keySetView = data.keySet();
			Iterator<Integer> iterator = keySetView.iterator();

			long time = System.currentTimeMillis();
			String output = time + ": " + name + ": ";

			while (iterator.hasNext()) {
				Integer key = iterator.next();
				String value = data.getOrDefault(key, "");
				output += key + "=>" + value + "; ";
			}

			System.out.println(output);

			try {
				Thread.sleep(300);
			} catch (InterruptedException ex) {
				ex.printStackTrace();
			}
		}
	}
}
