package example.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import example.data.Address;

public interface AddressRepository extends JpaRepository<Address, Long> {

}
