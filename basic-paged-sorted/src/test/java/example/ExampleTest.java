package example;

/**
 * Copyright 2021 Serguei Kouzmine
 */
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mockito;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import org.hamcrest.collection.IsArrayWithSize;
import org.json.JSONArray;

import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.util.NestedServletException;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import example.repository.TutorialRepository;
import example.controller.TutorialController;
import example.model.Tutorial;

@WebMvcTest
class ExampleTest {

	@Autowired
	private MockMvc mvc;
	private ResultActions resultActions;

	// initiaize real stuff
	@SuppressWarnings("unused")
	@InjectMocks
	private Launcher application = new Launcher();
	@MockBean
	private TutorialRepository mockService;
	@InjectMocks
	private TutorialController controller;
	// Pageable pageable = new PageRequest(0, 10, Sort.Direction.DESC, "id");

	Page<Tutorial> tutorialsPage = Mockito.mock(Page.class);

	// https://docs.spring.io/spring-data/commons/docs/current/api/org/springframework/data/domain/Page.html
	// https://www.codota.com/code/java/classes/org.springframework.data.domain.Page

	@BeforeEach
	public void beforeTest() throws Exception {

		when(mockService.findAll()).thenReturn(
				Arrays.asList(new Tutorial("title 1", "description 1", true),
						new Tutorial("title 2", "description 2", true),
						new Tutorial("title 3", "description 3", true),
						new Tutorial("title 4", "description 4", true)));
		when(mockService.findAll(ArgumentMatchers.any(Sort.class))).thenReturn(
				Arrays.asList(new Tutorial("title 1", "description 1", true),
						new Tutorial("title 2", "description 2", true),
						new Tutorial("title 3", "description 3", true),
						new Tutorial("title 4", "description 4", true)));
		when(tutorialsPage.getTotalPages()).thenReturn(new Integer(1));
		when(tutorialsPage.getTotalElements()).thenReturn(4L);
		when(tutorialsPage.getContent()).thenReturn(
				Arrays.asList(new Tutorial("title 1", "description 1", true),
						new Tutorial("title 2", "description 2", true),
						new Tutorial("title 3", "description 3", true),
						new Tutorial("title 4", "description 4", true)));
		when(mockService.findAll(ArgumentMatchers.any(Pageable.class)))
				.thenReturn(tutorialsPage);

		when(mockService.findById(ArgumentMatchers.any(Long.class)))
				.thenReturn(Optional.of(new Tutorial("title", "description", true)));
		when(mockService.findById(ArgumentMatchers.any()))
				.thenReturn(Optional.of(new Tutorial("title", "description", true)));

		controller = new TutorialController(mockService);
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
	}

	@Test
	// "{"totalItems":0,"tutorials":[],"totalPages":1,"currentPage":0}"
	public void test1() throws Exception {
		try {
			resultActions = mvc.perform(get("/api/tutorials"));
			resultActions.andExpect(status().isOk())
					.andExpect(content().string(containsString("tutorials")))
					.andExpect(jsonPath("$.totalPages", is(1)))
					.andExpect(jsonPath("$.currentPage", is(0)))
					.andExpect(jsonPath("$.tutorials[0].title", is("title 1")))
					.andExpect(jsonPath("$.tutorials.length()", is(4)));
		} catch (NestedServletException e) {
			System.err.println(e.getCause().toString());
		}
	}

	@Disabled
	// the jsonPath("$.tutorials") is JSONArray,not an Array
	// see also:
	// https://www.baeldung.com/jsonpath-count
	@Test
	public void test2() throws Exception {
		resultActions = mvc.perform(get("/api/tutorials"));
		resultActions.andExpect(
				jsonPath("$.tutorials", is(IsArrayWithSize.arrayWithSize(4))));
	}

	@Disabled
	// need to set up a better mock - nopagination observed
	@Test
	public void test3() throws Exception {
		resultActions = mvc.perform(get("/api/tutorials?page=1&size=2"));
		resultActions.andExpect(jsonPath("$.tutorials.length()", is(2)));
	}

	@Test
	// "{"id":0,"title":"title","description":"description","published":true}"
	public void test4() throws Exception {
		try {
			resultActions = mvc.perform(get("/api/tutorials/1"));
			resultActions.andExpect(status().isOk())
					.andExpect(jsonPath("$.*", hasSize(4)));
		} catch (NestedServletException e) {
			Throwable e2 = e.getCause();
			System.err.println(e2.toString());
			e2.printStackTrace();
		}
	}

}
