package example.domain.user;

public interface UserRepository {
	String create(NewUser user);
	User get(String login);
}
