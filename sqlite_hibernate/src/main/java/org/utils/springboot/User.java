package org.utils.springboot;

import javax.persistence.Column;
import javax.persistence.ColumnResult;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import java.io.Serializable;

@Entity
public class User implements Serializable {

	private static final long serialVersionUID = 1L;
	@Id
	@GeneratedValue
	private Long id;
	@Column(name = "name")
	private String userName;
	@Column(name = "password")
	private String passWord;
	@Column(name = "gender")
	private UserGenderEnum userGender;
	private String nickName;

	public User() {
		super();
	}

	public User(String userName, String passWord, UserGenderEnum userGender) {
		super();
		this.passWord = passWord;
		this.userName = userName;
		this.userGender = userGender;
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

	public String getPassWord() {
		return passWord;
	}

	public void setPassWord(String passWord) {
		this.passWord = passWord;
	}

	public UserGenderEnum getUserGender() {
		return userGender;
	}

	public void setUserGender(UserGenderEnum userGender) {
		this.userGender = userGender;
	}

	public String getNickName() {
		return nickName;
	}

	public void setNickName(String nickName) {
		this.nickName = nickName;
	}

	@Override
	public String toString() {
		return "userName " + this.userName + ", pasword " + this.passWord
				+ "gender " + userGender.name();
	}

}