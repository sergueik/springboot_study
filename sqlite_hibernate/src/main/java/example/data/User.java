package example.data;

import java.io.Serializable;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import example.data.Address;

// https://www.sqlite.org/datatype3.html
@Entity
@Table(name = "\"student\"")
public class User implements Serializable {
        private Address address;
	@Id
	@GeneratedValue
	private Long id;

	@Column(name = "name", columnDefinition = "TEXT")
	private String userName;

	@Column(name = "password", columnDefinition = "BLOB")
	private String password;

	@Column(name = "gender", columnDefinition = "INTEGER")
	private Gender gender;

	private String nickName;

	public User() {
		super();
	}

	public User(String userName, String password, Gender gender, 
			Address address) {
		super();
		this.password = password;
		this.userName = userName;
		this.gender = gender;
		this.address = address;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String data) {
		this.password = data;
	}

	public Gender getGender() {
		return gender;
	}

	public void setGender(Gender data) {
		this.gender = data;
	}

	public String getNickName() {
		return nickName;
	}

	public void setNickName(String nickName) {
		this.nickName = nickName;
	}

	@Override
	public String toString() {
		return "userName: " + this.userName + ", pasword: " + this.password
				+ "gender: " + gender.name();
	}

	@Column(name = "address_id", columnDefinition = "INTEGER")
	@OneToOne(cascade = CascadeType.ALL)
	public Address getAddress() {
		return this.address;
	}

	public void setAddress(Address address) {
		this.address = address;
	}
}
