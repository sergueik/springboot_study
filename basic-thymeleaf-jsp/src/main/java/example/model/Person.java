package example.model;

// import org.springframework.boot.context.properties.ConfigurationProperties;
// import org.springframework.boot.context.properties.ConstructorBinding;
// import org.springframework.boot.context.properties.EnableConfigurationProperties;
// import org.springframework.context.annotation.Configuration;

public class Person implements LoggedOnUser {

	private String letter = null;
	private String name = null;
	private Integer id = 0;

	public Person() {
	}

	public Person(Integer id, String letter, String name) {
		this.id = id;
		this.name = name;
		this.letter = letter;
	}

	public String getLetter() {
		return letter;
	}

	public void setLetter(String data) {
		letter = data;
	}

	public String getName() {
		return name;
	}

	public void setName(String data) {
		name = data;
	}

	public Integer getId() {
		return id;
	}

	public void setId(Integer data) {
		id = data;
	}

	private String loggedOnUserName;

	public String getLoggedOnUserName() {
		return loggedOnUserName;
	}

	public void setLoggedOnUserName(String value) {
		loggedOnUserName = value;
	}

}
