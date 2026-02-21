package example.service;

import example.exceptions.BusinessRuleViolationException;
import org.junit.jupiter.api.Test;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.is;

import static org.hamcrest.Matchers.containsString;

import static org.junit.jupiter.api.Assertions.assertThrows;

class OrderServiceTest {

	private final OrderService service = new OrderService();

	@Test
	void shouldThrowBusinessRuleViolationWhenQuantityTooHigh() {
		BusinessRuleViolationException e = assertThrows(BusinessRuleViolationException.class,
				() -> service.placeOrder(20));

		assertThat(e.getMessage(), containsString("Several business rules violated"));
		assertThat(e.getErrors(), notNullValue());
		assertThat(e.getErrors().size(), greaterThan(0));

		assertThat(e.getErrors().get(0).getRule(), is("MAX_ORDER_QUANTITY"));
		assertThat(e.getErrors().get(0).getMessage(), containsString("Order quantity must not exceed 10 items"));
	}
}