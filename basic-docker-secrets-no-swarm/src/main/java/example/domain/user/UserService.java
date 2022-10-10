package example.domain.user;

public class UserService {
	private final UserRepository userRepository;

	public UserService(UserRepository data) {
		userRepository = data;
	}

	public String create(NewUser user) {
		return userRepository.create(user);
	}

	public User get(String login) {
		return userRepository.get(login);
	}

}
