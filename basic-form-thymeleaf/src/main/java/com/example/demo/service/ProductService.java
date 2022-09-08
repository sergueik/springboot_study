package com.example.demo.service;

import com.example.demo.model.Product;
import com.example.demo.repository.ProductRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@Slf4j
public class ProductService {
	@Autowired
	private ProductRepository repo;
	
	public List<Product> listAll(String keyword) {
		if (keyword != null) {
			log.info("Поиск продукта по ключевому слову {}", keyword);
			return repo.search(keyword);
		}
		return repo.findAll();
	}
	
	public void save(Product product) {
		if (product != null){
			repo.save(product);
		}else {
			log.error("Продукт не может быть пустым!");
		}
	}
	
	public Product get(Long id) {
		if (repo.findById(id).isPresent()){
			return repo.findById(id).get();
		}
		log.error("Продукт не найден");
		return null;
	}
	
	public void delete(Long id) {
		log.warn("Продукт по id {} удален", id);
		repo.deleteById(id);
	}
}
