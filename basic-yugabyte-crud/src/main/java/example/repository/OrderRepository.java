package example.repository;

import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import example.model.Order;

@Repository
public interface OrderRepository extends JpaRepository<Order, UUID> {

}
