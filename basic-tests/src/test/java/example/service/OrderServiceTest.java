package example.service;

import example.exceptions.BusinessRuleViolationException;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class OrderServiceTest {

	private final OrderService service = new OrderService();

	@Test
	void shouldThrowBusinessRuleViolationWhenQuantityTooHigh() {
		BusinessRuleViolationException e = assertThrows(BusinessRuleViolationException.class,
				() -> service.placeOrder(20));

		assertEquals("MAX_ORDER_QUANTITY", e.getRule());
		assertEquals("Order quantity must not exceed 10 items", e.getMessage());
	}
}