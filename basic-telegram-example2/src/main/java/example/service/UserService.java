package example.service;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import example.model.User;
import example.repo.JpaUserRepository;

import javax.transaction.Transactional;
import java.util.List;
import java.util.Optional;

@Service
@Transactional
@RequiredArgsConstructor
public class UserService {

	private final JpaUserRepository userRepository;

	@Value("${bot.url}")
	String url;

	public User update(User user) {
		String ref = url + "?start=" + user.getUserId();
		user.setReferencesUrl(ref);
		return userRepository.save(user);
	}

	public void deleteById(Integer id) {
		User user = get(id).get();
		userRepository.deleteById(id);
	}

	public User getOrCreate(Integer id) {
		return get(id).orElseGet(() -> update(new User(id)));
	}

	public Optional<User> get(int chatId) {
		return userRepository.findById(chatId);
	}

	public List<User> getUsers() {
		return userRepository.findAll();
	}

	public int countReferals(int id) {
		return userRepository.countReferals(id);
	}

	public List<User> findAll() {
		return userRepository.findAll();
	}

	public List<User> findAllRefers(int id) {
		return userRepository.findAllRefers(id);
	}

	public List<User> findUsersWithRefer() {
		return userRepository.usersWithRefer();
	}

	public List<User> findByUsername() {
		return userRepository.findUsersWithName();
	}

	public List<User> findUsersByOilTime() {
		return userRepository.findUsersByOilTime();
	}

	public List<User> findUsersByElectricTime() {
		return userRepository.findUsersByElectricTime();
	}

	public User findByName(String name) {
		return userRepository.findByName(name);
	}
}
