package example;

import java.util.concurrent.*;

import java.nio.file.Paths;
import java.io.IOException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import java.util.concurrent.TimeUnit;

public class App {
 private static BlockingQueue<byte[]> queue = new LinkedBlockingQueue<>();

    public static void main(String[] args) {

        Runnable producer = () -> {
            while (true) {
                // generates 1mb of object every 10ms
                queue.offer(new byte[1 * 1024 * 1024]);
                try {
                    TimeUnit.MILLISECONDS.sleep(10);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        };

        Runnable consumer = () -> {
            while (true) {
                try {
                    // process every 100ms
                    queue.take();
                    TimeUnit.MILLISECONDS.sleep(100);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        };

        // give a name, good for profiling
        new Thread(producer, "Producer Thread").start();
        new Thread(consumer, "Consumer Thread").start();

    }

}
