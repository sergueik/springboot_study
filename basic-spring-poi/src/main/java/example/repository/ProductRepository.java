package example.repository;

import org.springframework.data.repository.CrudRepository;

import example.domain.Product;

public interface ProductRepository extends CrudRepository<Product, Long> {
}
