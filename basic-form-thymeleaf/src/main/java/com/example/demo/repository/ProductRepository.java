package com.example.demo.repository;


import com.example.demo.model.Product;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface ProductRepository extends JpaRepository<Product, Long> {

	@Query("SELECT p FROM Product p WHERE CONCAT(p.name, ' ', p.description, ' ', p.price) LIKE %?1%")
	List<Product> search(String keyword);
}
