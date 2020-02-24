package example.handle;

import org.I0Itec.zkclient.IZkDataListener;
import org.I0Itec.zkclient.ZkClient;
import org.I0Itec.zkclient.exception.ZkMarshallingError;
import org.I0Itec.zkclient.serialize.ZkSerializer;

import java.nio.charset.StandardCharsets;

public class ZkSession{
    public static void main(String[] args) throws InterruptedException {
        ZkSession leader = new ZkSession();
        ZkSession follow1 = new ZkSession();
        ZkSession follow2 = new ZkSession();
        while(true){
            System.out.println(".....");
            Thread.sleep(1000);
        }
    }

    private static final String ZK_SERVER_ADDR = "192.168.42.128:2181,192.168.42.128:2182,192.168.42.128:2183";

    private final ZkClient zkClient;

    public ZkSession(){
        zkClient = new ZkClient(ZK_SERVER_ADDR,5000);
        // unless serialization object is set, the default serialization object is used, and an IO exception may occur.
        zkClient.setZkSerializer(new ZkSerializer() {
            @Override
            public Object deserialize(byte[] bytes) throws ZkMarshallingError {
                return new String(bytes, StandardCharsets.UTF_8);
            }

            @Override
            public byte[] serialize(Object obj) throws ZkMarshallingError {
                return String.valueOf(obj).getBytes(StandardCharsets.UTF_8);
            }
        });
        this.register();
    }


    /**
      If there is no master, register as master, otherwise, register as worker
     */
    private void register(){
        if(zkClient.exists("/master")){
            if(!zkClient.exists("/workers")){
                zkClient.createPersistent("/workers");
            }

            zkClient.createEphemeralSequential("/workers/worker-","worker");

            zkClient.subscribeDataChanges("/master", this.getMasterListener());
        } else {
            System.out.println("master was successfully created: " + this);
            zkClient.createEphemeral("/master","master");
        }
    }

    /**
     master listener, master changes output changes and keeps listening, master deletes and re-registers master
     */
    private IZkDataListener getMasterListener(){
        return new IZkDataListener() {
            @Override
            public void handleDataChange(String dataPath, Object data){
                System.out.println("master content modification, keep listening");
                zkClient.subscribeDataChanges("/master",this);
            }

            @Override
            public void handleDataDeleted(String dataPath) {
                System.out.println("master has been deleted, register new master`");
                ZkSession.this.register();
            }
        };
    }
}


