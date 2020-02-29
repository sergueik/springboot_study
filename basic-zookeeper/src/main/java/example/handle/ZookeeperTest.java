package example.handle;

import org.apache.zookeeper.ZooKeeper;

import example.watcher.MyWatcher;

import java.io.IOException;

public class ZookeeperTest {
    public static void main(String[] args) throws IOException, InterruptedException {
        ZooKeeper zooKeeper = new ZooKeeper("192.168.42.128:2181,192.168.42.128:2182,192.168.42.128:2183",5000,new MyWatcher());
        while (true) {
            Thread.sleep(1000);
            System.out.println(zooKeeper.getState());
        }

    }
}
