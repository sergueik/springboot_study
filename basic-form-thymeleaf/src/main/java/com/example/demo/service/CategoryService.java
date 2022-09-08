package com.example.demo.service;

import com.example.demo.model.Category;
import com.example.demo.model.Product;
import com.example.demo.repository.CategoryRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class CategoryService{
    private final CategoryRepository repository;

    public List<Category> listAll(String keyword) {
        if (keyword != null) {
            log.info("Поиск продукта по ключевому слову {}", keyword);
            return repository.search(keyword);
        }
        return repository.findAll();
    }

    public void save(Category category) {
        if (category != null){
            repository.save(category);
        }else {
            log.error("Категория не может быть пустым!");
        }
    }

    public Category get(Long id) {
        if (repository.findById(id).isPresent()){
            return repository.findById(id).get();
        }
        log.error("Категория не найдено");
        return null;
    }

    public void delete(Long id) {
        log.warn("Категория по id {} удален", id);
        repository.deleteById(id);
    }

    public List<Category> getAll(){
        return repository.findAll();

    }

}
