package example.data.user;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import example.domain.user.NewUser;
import example.domain.user.User;
import example.domain.user.UserRepository;

public class InMemoryUserRepository implements UserRepository {

	@SuppressWarnings("rawtypes")
	private static final Map<String, User> USERS_STORE = new ConcurrentHashMap<String, User>();

	@SuppressWarnings("unchecked")
	@Override
	public String create(NewUser data) {
		final String id = UUID.randomUUID().toString();
		User user = User.builder().id(id).login(data.getLogin())
				.password(data.getPassword()).build();
		USERS_STORE.put(data.getLogin(), user);

		return id;
	}

	@Override
	public User get(String login) {
		return USERS_STORE.get(login);
	}
}
