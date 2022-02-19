package example.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Service;

import example.model.OilPump;
import example.model.Powerhouse;
import example.repo.JpaPowerhouseRepository;

import javax.transaction.Transactional;
import java.util.List;

@Service
@Transactional
public class PowerhouseService {

    private JpaPowerhouseRepository powerhouseRepository;

    @Autowired
    public PowerhouseService(JpaPowerhouseRepository powerhouseRepository) {
        this.powerhouseRepository = powerhouseRepository;
    }

    public PowerhouseService() {
    }

    public Powerhouse findById(int id){
        return powerhouseRepository.findById(id).get();
    }

    public List<Powerhouse> findByUserId(int id){
        return powerhouseRepository.findByUserId(id);
    }

    public List<Powerhouse> findAll(){
        return powerhouseRepository.findAll();
    }

    public List<Powerhouse> findAllUsersPower(){
        return powerhouseRepository.findAllUserPower();
    }

    public Powerhouse save(Powerhouse powerhouse){
        return powerhouseRepository.save(powerhouse);
    }

    public List<Powerhouse> findByLevel(int level, int id){
        return powerhouseRepository.findByLevel(level, id);
    }

}
