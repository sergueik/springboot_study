package example.watcher;

import org.apache.zookeeper.WatchedEvent;
import org.apache.zookeeper.Watcher;

/*
  Zookeeper connected monitor
 Responsible for monitoring the health of the connection and can also be used to monitor changes in Zookeeper data
*/
public class MyWatcher implements Watcher {
    @Override
    public void process(WatchedEvent watchedEvent) {
        System.out.println(watchedEvent);
    }
}
