package example.controller;

/**
 * Copyright 2021 Serguei Kouzmine
 */

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import org.junit.runner.RunWith;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;

import org.hamcrest.Matcher;

import example.controller.UserController;
import example.data.Gender;
import example.data.Student;
import example.repository.StudentRepository;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import example.Launcher;
// https://github.com/TechPrimers/test-controller-example
// https://github.com/kriscfoster/spring-boot-testing-pyramid

@RunWith(SpringJUnit4ClassRunner.class)
// @WebMvcTest
public class MVCMockTest {

	final static String route = "/getUsers";
	final static String body = "Hello basic";
	final static String charset = "ISO-8859-1";
	private ResultActions resultActions;
	private MockMvc mvc;
	private final Student user = new Student("name", "password", Gender.MAN);
	@InjectMocks
	private Launcher application;

	@Mock
	private StudentRepository mockRepository;

	@InjectMocks
	private UserController controller;

	private static Matcher<String> matcher;

	@Before
	public void beforeTest() throws Exception {
		// the User class has no constructor with id and nickname
		user.setId(0L);
		// user.setNickName("nickname");
		when(mockRepository.findAll())
				.thenReturn(Arrays.asList(new Student[] { user }));
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
		resultActions = mvc.perform(get(route));
	}

	// examine HTTP status
	@Test
	public void statusTest() throws Exception {
		resultActions.andExpect(status().isOk());
	}

	// examine body
	@Test
	public void bodyTest() throws Exception {
		matcher = containsString(new Gson().toJson(user));
		resultActions.andExpect(content().string(matcher));
	}

	// examine body
	@Test
	public void bodyTest2() throws Exception {
		matcher = containsString(
				new Gson().toJson(Arrays.asList(new Student[] { user })));
		resultActions.andExpect(content().string(matcher));
	}

	// examine backend call
	@Test
	public void subjectMethodTest() throws Exception {
		verify(mockRepository).findAll();
	}

	// examine response header
	@Ignore
	// TODO: set up mock to produce desired headers
	@Test
	// NOTE: these expectations are Junit version sensitive
	public void contentTypeTest() throws Exception {
		// mvc.perform(get(route ).accept(MediaType.TEXT_PLAIN)).andExpect(
		// content().contentType(String.format("text/plain;charset=%s", charset)));
		mvc.perform(get(route + "/json").accept(MediaType.APPLICATION_JSON))
				.andExpect(content().contentType("application/json"));
	}

	// explore the content details
	@Test
	public void jsonTest() throws Exception {
		mvc.perform(get(route)).andExpect(jsonPath("$[0].userName", is("name")));
	}

}