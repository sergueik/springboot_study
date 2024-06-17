package com.keysoft.mongodb.repositories;

import com.keysoft.mongodb.model.Ticket;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.mongodb.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.stream.Stream;

@Repository
public interface TicketRepository extends MongoRepository<Ticket, String> {
    List<Ticket> findByStatus(String status);
    List<Ticket> findByAppId(String appId);

    @Query("{ 'status' : ?0 }")
    Stream<Ticket> findAllByCustomQueryAndStream(String status);

}
