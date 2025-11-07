package example.repository;

import example.model.User;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

public class UserRepository {
    private final Map<Long, User> users = new ConcurrentHashMap<>();

    public List<User> findAll() {
        return new ArrayList<>(users.values());
    }

    public Optional<User> findById(Long id) {
        return Optional.ofNullable(users.get(id));
    }

    public User save(User user) {
        if (user.getId() == null) {
            user.setId((long) (users.size() + 1));
        }
        users.put(user.getId(), user);
        return user;
    }

    public void delete(Long id) {
        users.remove(id);
    }
}

