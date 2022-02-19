package example.repo;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import example.model.Payment;

import java.util.List;

@Repository
public interface JpaPaymentRepository extends JpaRepository<Payment, Integer> {

    @Query("SELECT p from Payment p where p.success=false")
    List<Payment> findAllBySuccess();
}
