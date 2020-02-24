package example.handle;

import org.I0Itec.zkclient.ZkClient;
import org.I0Itec.zkclient.ZkConnection;
import org.apache.zookeeper.CreateMode;
import org.apache.zookeeper.KeeperException;
import org.apache.zookeeper.ZooKeeper;
import org.apache.zookeeper.data.Stat;

import example.watcher.MyWatcher;

import java.io.IOException;
import java.util.Random;

import static org.apache.zookeeper.ZooDefs.Ids.OPEN_ACL_UNSAFE;

public class Master {
	 private ZooKeeper zooKeeper;
    /**
Zookeeper cluster path, need to include all server addresses, otherwise the session object will not automatically switch and reconnect
     */
    private static final String ZOOKEEPER_SERVER_PATH = "192.168.42.128:2181,192.168.42.128:2182,192.168.42.128:2183";

    // Master node 
    private boolean isLeader;

    // Master node id
    private String serverId = Integer.toHexString(new Random().nextInt());

    public Master() throws IOException {
        zooKeeper = new ZooKeeper(ZOOKEEPER_SERVER_PATH,5000,new MyWatcher());
    }


    public void syncRunForMaster() throws KeeperException, InterruptedException {
        while(true){
            try{
               // Get master permissions by creating a master node
                zooKeeper.create("/master",serverId.getBytes(),OPEN_ACL_UNSAFE, CreateMode.EPHEMERAL);
                isLeader = true;
                break;
            }catch (KeeperException.NodeExistsException e){
                System.out.println("Master node already exists");
                isLeader = false;
                break;
            }catch (KeeperException.ConnectionLossException e){
                System.out.println("Network exception, requesting reconnection");
            }

            if(this.syncCheckMaster()){
                break;
            }
        }
    }

    private boolean syncCheckMaster() throws KeeperException, InterruptedException {
        while(true){
            try{
                byte[] data = zooKeeper.getData("/master",false,new Stat());
                isLeader = new String(data).equals(serverId);
                return true;
            }catch (KeeperException.NoNodeException e){
                System.out.println("There is no master node yet.");
                return false;
            }catch (KeeperException.ConnectionLossException e){
                System.out.println("Network exception, requesting reconnection");
            }
        }
    }



    public static void main(String[] args) throws IOException, KeeperException, InterruptedException {
        //
        String connectionAddr = "192.168.42.128:2181,192.168.42.128:2182,192.168.42.128:2183";

        ZkClient zkClient = new ZkClient(new ZkConnection(connectionAddr),5000);

        // Create master node and get management rights
        zkClient.createEphemeral("/master",Integer.toHexString(new Random().nextInt()));

        // Create metadata node
        zkClient.createPersistent("/workers","workers");
        zkClient.createPersistent("/tasks","tasks");
        zkClient.createPersistent("/assign","assign");
        zkClient.createPersistent("/status","status");

        // View node data
        System.out.println("workers node data:" + zkClient.readData("/workers"));
        System.out.println("tasks node data:" + zkClient.readData("/tasks"));
        System.out.println("assign node data:" + zkClient.readData("/assign"));
        System.out.println("status node data:" + zkClient.readData("/status"));

        // Register two slave nodes to perform tasks
        zkClient.createEphemeralSequential("/workers/worker-","Idle");
        zkClient.createEphemeralSequential("/workers/worker-","Idle");

        // get and dump worker data
        for(String worker : zkClient.getChildren("/workers")){
            System.out.println(worker + " : " + zkClient.readData("/workers/" + worker));
        }

        // Registration task
        zkClient.createPersistentSequential("/tasks/task-","command1");
        zkClient.createPersistentSequential("/tasks/task-","command2");

        // retrieve and dump task node data
        for(String task : zkClient.getChildren("/tasks")){
            System.out.println(task + " : " + zkClient.readData("/tasks/" + task));
        }


        // recursively delete permanent node
        zkClient.deleteRecursive("/workers");
        zkClient.deleteRecursive("/tasks");
        zkClient.deleteRecursive("/assign");
        zkClient.delete("/status");
    }
}
