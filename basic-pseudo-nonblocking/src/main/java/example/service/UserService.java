package example.service;

import example.model.User;
import example.repository.UserRepository;
import org.springframework.stereotype.Service;
import java.util.List;
import java.util.Optional;

@Service
public class UserService {
    private final UserRepository repo = new UserRepository();

    public List<User> getAllUsers() {
        return repo.findAll();
    }

    public Optional<User> getUser(Long id) {
        return repo.findById(id);
    }

    public User createUser(User user) {
        return repo.save(user);
    }

    public void deleteUser(Long id) {
        repo.delete(id);
    }
}

