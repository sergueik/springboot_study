package example.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import example.model.Product;

@Repository
public interface ProductRepository extends JpaRepository<Product, Long> {
}
