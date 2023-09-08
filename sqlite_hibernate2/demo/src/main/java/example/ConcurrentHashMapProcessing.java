package example;

import java.util.concurrent.ConcurrentHashMap;

import example.service.ReaderService;
import example.service.WriterService;


public class ConcurrentHashMapProcessing {

	public static void main(String[] args) {
		ConcurrentHashMap<Integer, String> map = new ConcurrentHashMap<>();

		new WriterService(map, "Writer-1", 1).start();
		new WriterService(map, "Writer-2", 2).start();

		for (int i = 1; i <= 5; i++) {
			new ReaderService(map, "Reader-" + i).start();
		}
	}
}
