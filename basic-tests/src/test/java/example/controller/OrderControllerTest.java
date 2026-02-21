package example.controller;

import example.exceptions.BusinessRuleViolationException;
import example.handler.GlobalExceptionHandler;
import example.service.OrderService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.http.HttpStatus;
import org.springframework.test.web.servlet.MockMvc;

import static org.mockito.Mockito.doThrow;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

import java.util.ArrayList;
import java.util.List;

import org.assertj.core.util.Arrays;

import example.handler.dto.BusinessError;

@WebMvcTest(OrderController.class)
@Import(GlobalExceptionHandler.class)
class OrderControllerTest {

	@Autowired
	private MockMvc mockMvc;

	@MockBean
	private OrderService orderService;

	@Test
	void shouldReturn422WhenBusinessRuleViolated() throws Exception {
		List<BusinessError> errors = new ArrayList<>();
		errors.add(new BusinessError("MAX_ORDER_QUANTITY", "Order quantity must not exceed 10 items"));
		doThrow(new BusinessRuleViolationException(errors)).when(orderService).placeOrder(20);

		mockMvc.perform(post("/orders").param("quantity", "20")).andExpect(status().isUnprocessableEntity())
				.andExpect(jsonPath("$.title").value("Business rule violation"))
				.andExpect(jsonPath("$.status").value(HttpStatus.UNPROCESSABLE_ENTITY.value()))
				.andExpect(jsonPath("$.errors").isArray())
				.andExpect(jsonPath("$.errors[0].message").value("Order quantity must not exceed 10 items"))
				.andExpect(jsonPath("$.errors[0].rule").value("MAX_ORDER_QUANTITY"));
	}
}