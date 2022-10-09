package example.domain.user;

public class UserService {
	private final UserRepository userRepository;

	public UserService(UserRepository userRepository) {
		this.userRepository = userRepository;
	}

	public String create(NewUser user) {
		return userRepository.create(user);
	}

}
