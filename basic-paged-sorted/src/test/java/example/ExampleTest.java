package example;

/**
 * Copyright 2021 Serguei Kouzmine
 */
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.Page;
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
	private TutorialController controller; // = new
																					// TutorialController(mockService);

	// https://docs.spring.io/spring-data/commons/docs/current/api/org/springframework/data/domain/Page.html
	// https://www.codota.com/code/java/classes/org.springframework.data.domain.Page

	@BeforeEach
	public void beforeTest() throws Exception {

		when(mockService.findAll()).thenReturn(new ArrayList<Tutorial>());
		when(mockService.findAll(ArgumentMatchers.any(Sort.class)))
				.thenReturn(new ArrayList<Tutorial>());
		when(mockService.findAll(ArgumentMatchers.any(Pageable.class)))
				.thenReturn(Page.empty());

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
					.andExpect(jsonPath("$.totalPages", is(1)))
					.andExpect(jsonPath("$.currentPage", is(0)))
					.andExpect(jsonPath("$.tutorials", is(new ArrayList<String>())))
					.andExpect(content().string(containsString("{")));
		} catch (NestedServletException e) {
			System.err.println(e.getCause().toString());
		}
	}

	@Test
	// "{"id":0,"title":"title","description":"description","published":true}"
	public void test2() throws Exception {
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
