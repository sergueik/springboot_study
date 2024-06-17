package com.keysoft.mongodb.service;

import com.keysoft.mongodb.model.Release;
import com.keysoft.mongodb.repositories.ReleaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class ReleaseServiceImpl implements ReleaseService{
    @Autowired
    private MongoTemplate mongoTemplate;

    @Autowired
    private ReleaseRepository releaseRepository;

    public List<Release> getReleaseByTicketStatus(String status) {
        Query query = new Query();
        query.addCriteria(Criteria.where("tickets.status").is(status)); //embedded document use tickets.status

        //repository is not flexible enough to do this, so using mongoTemplate
        return mongoTemplate.find(query, Release.class);
    }

    @Override
    public List<Release> findAll() {
        return releaseRepository.findAll();
    }

    @Override
    public Optional<Release> findById(String id) {
        return releaseRepository.findById(id);
    }

    @Override
    public Release save(Release release) {
        return releaseRepository.save(release);
    }

    @Override
    public void deleteById(String id) {
        releaseRepository.deleteById(id);
    }

    @Override
    public void insert(Release release) {
        releaseRepository.insert(release);
    }

    @Override
    public Double getCosts(String id) {
        Double cost = 0.00;
        Optional<Release> release = findById(id);
        if(release.isPresent()) cost = release.get().getEstimatedCosts();
        return cost;
    }
}
