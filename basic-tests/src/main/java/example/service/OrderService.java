package example.service;

import example.exceptions.BusinessRuleViolationException;
import org.springframework.stereotype.Service;

@Service
public class OrderService {

    public void placeOrder(int quantity) {
        if (quantity > 10) {
            throw new BusinessRuleViolationException(
                    "MAX_ORDER_QUANTITY",
                    "Order quantity must not exceed 10 items"
            );
        }
		// BAU logic here (persist, send event, etc.)
    }
}