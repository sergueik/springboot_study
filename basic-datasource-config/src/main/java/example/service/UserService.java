package example.service;

import org.springframework.stereotype.Service;

import example.model.User;

public interface UserService {

	User findByUsername(String username);
}
