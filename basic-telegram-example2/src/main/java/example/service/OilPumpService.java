package example.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import example.model.OilPump;
import example.repo.JpaOilPumpRepository;

import javax.transaction.Transactional;
import java.util.List;

@Service
@Transactional
public class OilPumpService {

    private JpaOilPumpRepository oilPumpRepository;

    @Autowired
    public OilPumpService(JpaOilPumpRepository oilPumpRepository) {
        this.oilPumpRepository = oilPumpRepository;
    }

    public OilPumpService() {
    }

    public OilPump findById(int id){
        return oilPumpRepository.findById(id).get();
    }

    public List<OilPump> findByUserId(int id){
        return oilPumpRepository.findByUserId(id);
    }

    public List<OilPump> findAll(){
        return oilPumpRepository.findAll();
    }

    public List<OilPump> findAllByUsersOil(){
        return oilPumpRepository.findAllByUsersOil();
    }

    public OilPump save(OilPump oilPump){
        return oilPumpRepository.save(oilPump);
    }

    public List<OilPump> findByLevel(int level, int id){
        return oilPumpRepository.findByLevel(level, id);
    }
}
