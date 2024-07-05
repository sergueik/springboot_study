package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasKey;

import java.util.Set;
import java.util.HashSet;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Arrays;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.google.gson.Gson;

//origin: https://www.baeldung.com/java-serialization

public class SerializationTest {
	private boolean debug = true;
	private Person person;
	private static Gson gson = new Gson();

	private static class Person implements Serializable {
		private static final long serialVersionUID = 1L;
		private String name;
		private Address address; // must be serializable too
		transient private int age;

		public int getAge() {
			return age;
		}

		public void setAge(int value) {
			age = value;
		}

		public String getName() {
			return name;
		}

		public void setName(String value) {
			name = value;
		}

		public Address getAddress() {
			return address;
		}

		public void setAddress(Address value) {
			address = value;
		}

	}

	public static class Address implements Serializable {
		private int houseNumber;

		void setHouseNumber(int value) {
			houseNumber = value;
		}

		int getHouseNumber() {
			return houseNumber;
		}

	}

	@Before
	public void before() {
		person = new Person();
		person.setAge(20);
		person.setName("Joe");

		Address address = new Address();
		address.setHouseNumber(1);
		person.setAddress(address);
	}

	@Test
	// https://stackoverflow.com/questions/8436688/memory-stream-in-java
	// https://www.baeldung.com/java-outputstream
	// https://www.baeldung.com/java-outputstream-byte-array
	public void test1() throws IOException, ClassNotFoundException {

		ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
		ObjectOutputStream objectOutputStream = new ObjectOutputStream(byteArrayOutputStream);
		objectOutputStream.writeObject(person);
		objectOutputStream.flush();
		objectOutputStream.close();
		byte[] data = byteArrayOutputStream.toByteArray();
		// System.err.println(new String(data));
		// will be a binary
		ByteArrayInputStream inputStream = new ByteArrayInputStream(data);

		ObjectInputStream objectInputStream = new ObjectInputStream(inputStream);
		Person p2 = (Person) objectInputStream.readObject();
		objectInputStream.close();

		assertThat(p2.getAge(), is(0));
		assertThat(p2.getName(), is(person.getName()));
		assertThat(p2.getAddress().getHouseNumber(), is(person.getAddress().getHouseNumber()));

	}

	@Test
	public void test2() throws IOException, ClassNotFoundException {

		String json = gson.toJson(person);
		System.err.println(json);

		Person p2 = gson.fromJson(json, Person.class);
		assertThat(p2.getAge(), is(0));
		assertThat(p2.getName(), is(person.getName()));
		assertThat(p2.getAddress().getHouseNumber(), is(person.getAddress().getHouseNumber()));

	}

}
