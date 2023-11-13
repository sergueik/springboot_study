package example.dao.product;

import java.util.List;

import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.PagingAndSortingRepository;

import example.model.product.Product;

public interface ProductRepository extends PagingAndSortingRepository<Product, Integer> {


    List<Product> findAllByPrice(double price, Pageable pageable);
}
