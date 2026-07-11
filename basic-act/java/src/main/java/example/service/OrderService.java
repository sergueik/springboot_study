package example.service;

import example.exceptions.BusinessRuleViolationException;

import java.util.ArrayList;
import java.util.List;

import org.springframework.stereotype.Service;
import example.handler.dto.BusinessError;

@Service
public class OrderService {

	public void placeOrder(int quantity) {
		List<BusinessError> violations = new ArrayList<>();

		if (quantity > 10) {
			violations.add(new BusinessError(
					"MAX_ORDER_QUANTITY", "Order quantity must not exceed 10 items"));
			throw new BusinessRuleViolationException(violations);
		}
		// BAU logic here (persist, send event, etc.)
	}
}