package example.controller;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import example.controller.ExampleController;
import example.service.ExampleService;

import static org.junit.jupiter.api.Assertions.assertEquals;

// import static org.mockito.Mockito.when;
import org.mockito.Mockito;

class UnitTest {

	ExampleController controller;
	ExampleService mockService;

	@BeforeEach
	void setup() {
		mockService = Mockito.mock(ExampleService.class);
		Mockito.when(mockService.hello()).thenReturn("mock");
		controller = new ExampleController(mockService);
	}

	@Test
	void shouldGetDefaultWelcomeMessage() {
		assertEquals("mock", controller.hello());
	}

}
