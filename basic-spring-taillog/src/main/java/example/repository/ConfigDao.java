package example.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import example.entity.Config;

public interface ConfigDao extends JpaRepository<Config, Integer> {
}
