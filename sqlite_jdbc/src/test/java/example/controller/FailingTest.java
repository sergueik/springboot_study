package example.controller;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Copyright 2021 Serguei Kouzmine
 */

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import org.mockito.exceptions.verification.WantedButNotInvoked;

import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import example.Application;
import example.service.BaseService;


@RunWith(SpringJUnit4ClassRunner.class)
//@WebMvcTest
public class FailingTest {
	final static String route = "/student/findAllStudent";
	private ResultActions resultActions;
	private MockMvc mvc;

	@InjectMocks
	private Application application;

	@InjectMocks
	private Controller controller;
	@Mock
	private BaseService mockService;

	@Before
	public void beforeTest() throws Exception {
		when(mockService.findAllStudent()).thenReturn(null);
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
		resultActions = mvc.perform(get(route));
	}

	// examine HTTP status
	@Test
	public void statusTest() throws Exception {
		resultActions.andExpect(status().isMethodNotAllowed());
	}

	// examine body
	@Test
	public void bodyTest() throws Exception {
		resultActions.andExpect(content().string(""));
	}

	// Actually, there were zero interactions with this mock.
	@Test(expected = WantedButNotInvoked.class)
	public void subjectMethodTest() throws Exception {
		verify(mockService).findAllStudent();
	}

}
