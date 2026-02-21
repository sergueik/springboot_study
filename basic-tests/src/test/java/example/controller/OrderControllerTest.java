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

@WebMvcTest(OrderController.class)
@Import(GlobalExceptionHandler.class)
class OrderControllerTest {

	@Autowired
	private MockMvc mockMvc;

	@MockBean
	private OrderService orderService;

	@Test
	void shouldReturn422WhenBusinessRuleViolated() throws Exception {

		doThrow(new BusinessRuleViolationException("MAX_ORDER_QUANTITY", "Order quantity must not exceed 10 items"))
				.when(orderService).placeOrder(20);

		mockMvc.perform(post("/orders").param("quantity", "20")).andExpect(status().isUnprocessableEntity())
				.andExpect(jsonPath("$.title").value("Business rule violation"))
				.andExpect(jsonPath("$.status").value(HttpStatus.UNPROCESSABLE_ENTITY.value()));
	}
}