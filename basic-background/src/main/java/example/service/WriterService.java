package example.service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.ConcurrentMap;

import org.springframework.beans.factory.annotation.Autowired;

import example.component.DataComponent;
import example.domain.Gender;
import example.domain.User;
import example.repository.UserRepository;

public class WriterService extends Thread {
	private DataComponent data;
	private Random random;
	private String name;
	private Long index = 1L;

	UserRepository userRepository;

	public WriterService(UserRepository userRepository, DataComponent data,
			String name, long randomSeed) {
		this.data = data;
		this.userRepository = userRepository;
		this.random = new Random(randomSeed);
		this.name = name;
	}

	public void run() {
		while (true) {

			Map<Long, User> cachedUsers = new HashMap<>();
			List<User> users = userRepository.findAll();
			// only update 10 users
			int maxcnt = 100;
			int cnt = 0;
			for (User user : users) {
				if (cnt++ > maxcnt) {
					break;
				}
				cachedUsers.put(user.getId(), user);
			}
			data.put(index, cachedUsers);

			System.out.println(String.format("%d: %s has updated users",
					System.currentTimeMillis(), name));

			try {
				Thread.sleep(5000);
			} catch (InterruptedException ex) {
				ex.printStackTrace();
			}
		}
	}
}
