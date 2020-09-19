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

	// turn out both "user_name" and "name" columns are there in the "user" table:
	/*
	 sqlite3.exe
	
	sqlite> .open c:/Users/Serguei/sqlite/springboot.db
	sqlite> .tables
	student  user
	
	sqlite> .schema user
	CREATE TABLE user (id  integer, nick_name varchar, pass_word varchar, user_name
	varchar, user_sex integer, user_gender integer, password varchar, gender integer
	, name varchar, primary key (id));
	 */
	// possibly result of a bad migration
	@Column(name = "name")
	private String userName;

	@Column(name = "password")
	private String password;

	@Column(name = "gender")
	private Gender gender;

	private String nickName;

	public User() {
		super();
	}

	public User(String userName, String password, Gender gender) {
		super();
		this.password = password;
		this.userName = userName;
		this.gender = gender;
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

}