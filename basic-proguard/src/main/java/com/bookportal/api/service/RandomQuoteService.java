package com.bookportal.api.service;

import com.bookportal.api.entity.RandomQuote;
import com.bookportal.api.exception.CustomNotFoundException;
import com.bookportal.api.model.enums.ExceptionItemsEnum;
import com.bookportal.api.repository.RandomQuoteRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.data.mongodb.core.ReactiveMongoTemplate;
import org.springframework.data.mongodb.core.aggregation.Aggregation;
import org.springframework.data.mongodb.core.aggregation.SampleOperation;
import org.springframework.data.mongodb.core.aggregation.TypedAggregation;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Service
@RequiredArgsConstructor
public class RandomQuoteService {
    private final RandomQuoteRepository randomQuoteRepository;
    private final ReactiveMongoTemplate mongoTemplate;

    public Mono<RandomQuote> getRandomQuote() {
        return randomQuoteRepository.count()
                .map(aLong -> {
                    SampleOperation sampleOperation = new SampleOperation(1);
                    TypedAggregation<RandomQuote> typedAggregation = Aggregation.newAggregation(RandomQuote.class, sampleOperation);
                    return mongoTemplate.aggregate(typedAggregation, mongoTemplate.getCollectionName(RandomQuote.class), RandomQuote.class);
                })
                .flatMap(Flux::collectList)
                .filter(randomQuotes -> randomQuotes.size() != 0)
                .switchIfEmpty(Mono.error(new CustomNotFoundException(ExceptionItemsEnum.QUOTE.getValue())))
                .map(randomQuotes -> randomQuotes.get(0));
    }
}
