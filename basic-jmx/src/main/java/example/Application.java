package example;

import javax.management.*;
import java.lang.management.ManagementFactory;
import java.util.Queue;
import java.util.concurrent.ArrayBlockingQueue;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class Application {
	public static void main(String[] args)
			throws MalformedObjectNameException, NotCompliantMBeanException,
			InstanceAlreadyExistsException, MBeanRegistrationException {
	   MBeanServer mbs = ManagementFactory.getPlatformMBeanServer();
     ObjectName name = new ObjectName("mike.example:type=TodoList");
     TodoListMBean mbean = new TodoList();
     mbs.registerMBean(mbean, name);

     ObjectName mxbeanName = new ObjectName("mike.example:type=QueueSampler");

     Queue<String> queue = new ArrayBlockingQueue<String>(10);
     queue.add("Request-1");
     queue.add("Request-2");
     queue.add("Request-3");
     QueueSampler mxbean = new QueueSampler(queue);

     mbs.registerMBean(mxbean, mxbeanName);

		SpringApplication.run(Application.class, args);
	}
}
