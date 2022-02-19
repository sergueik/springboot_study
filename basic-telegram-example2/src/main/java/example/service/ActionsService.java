package example.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import example.model.Action;
import example.repo.JpaActionsRepository;

import java.util.List;


@Service
public class ActionsService {

    private final JpaActionsRepository actionsRepository;

    @Autowired
    public ActionsService(JpaActionsRepository actionsRepository) {
        this.actionsRepository = actionsRepository;
    }

    public Action update(Action actions){
        return actionsRepository.save(actions);
    }

    public List<Action> findByUserId(int id){
        return actionsRepository.findByUserId(id);
    }

    public Action findByUserIdAndName(int id, String name){
        return actionsRepository.findByUserIdAndName(id, name);
    }

    public List<Action> findAll(){
        return actionsRepository.findAll();
    }

}
