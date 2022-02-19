package example.repo;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import example.model.Channel;

@Repository
public interface JpaChannelRepository extends JpaRepository<Channel, String> {
}
