package example.service;

import org.springframework.stereotype.Service;

import example.model.Payment;
import example.repo.JpaPaymentRepository;

import javax.transaction.Transactional;
import java.util.List;

@Service
@Transactional
public class PaymentService {

    private final JpaPaymentRepository paymentRepository;

    public PaymentService(JpaPaymentRepository paymentRepository) {
        this.paymentRepository = paymentRepository;
    }

    public List<Payment> findAllPaymentsNotSuccess(){
        return paymentRepository.findAllBySuccess();
    }

    public Payment findById(Integer id){
        return paymentRepository.findById(id).get();
    }

    public Payment update(Payment payment){
        return paymentRepository.save(payment);
    }
}
