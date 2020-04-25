package example.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import example.entity.Item;

public interface ItemRepository extends JpaRepository<Item, Integer> {

}
