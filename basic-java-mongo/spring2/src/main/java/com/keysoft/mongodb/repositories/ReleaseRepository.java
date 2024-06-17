package com.keysoft.mongodb.repositories;

import com.keysoft.mongodb.model.Release;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ReleaseRepository extends MongoRepository<Release, String>{
}
