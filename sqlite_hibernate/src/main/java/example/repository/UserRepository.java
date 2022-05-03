package example.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import example.data.User;

public interface UserRepository extends JpaRepository<User, Long> {
	// run-time errors when parent project is Springboot 2.3.4-RELEASE:
	//  
	// org.springframework.beans.factory.UnsatisfiedDependencyException:
	// Error creating bean with name 'userController':
	// Unsatisfied dependency expressed through field 'userRepository';
	// nested exception is
	// org.springframework.beans.factory.BeanCreationException:
	// Error creating bean with name 'userRepository'
	// defined in example.repository.UserRepository defined in
	// @EnableJpaRepositories declared on Launcher: Invocation of init method
	// failed;
	// nested exception is java.lang.IllegalArgumentException:
	// Failed to create query for method public abstract example.data.User
	// example.repository.UserRepository.findOne(java.lang.Long)!
	// No property findOne found for type User!
	// public User findOne(Long id);
	// public List<User>findAl();

	// and

	// method findOne in interface
	// org.springframework.data.repository.query.QueryByExampleExecutor<T> cannot be
	// applied to given types;
	// [ERROR] required: org.springframework.data.domain.Example<S>
	// [ERROR] found: java.lang.Long

}
