package example.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import example.model.MySqlProduct;

public interface ProductMySqlRepository
		extends JpaRepository<MySqlProduct, Integer> {

}
