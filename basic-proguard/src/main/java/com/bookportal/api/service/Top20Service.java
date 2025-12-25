package com.bookportal.api.service;

import com.bookportal.api.entity.Book;
import com.bookportal.api.entity.Top20;
import com.bookportal.api.entity.Vote;
import com.bookportal.api.entity.softmodels.BookSoft;
import com.bookportal.api.exception.ExceptionResolver;
import com.bookportal.api.repository.Top20Repository;
import com.bookportal.api.repository.VoteRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
//the lombok.extern.slf4j.Sld4j does not work.
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.modelmapper.ModelMapper;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class Top20Service {
	private final Logger log = LoggerFactory.getLogger(Top20Service.class);
    private final Top20Repository top20Repository;
    private final BookService bookService;
    private final VoteRepository voteRepository;
    private final ModelMapper mapper = new ModelMapper();

    public Mono<List<Top20>> getTop20() {
        return top20Repository.findAll().collectList();
    }

    @CacheEvict(value = {"homePage"})
    public void updateTop20() {
        Map<String, Top20> top20Map = new HashMap<>();
        Optional<List<Vote>> optionalVoteList = voteRepository.findAll().collectList().blockOptional();
        optionalVoteList.ifPresent(voteList -> {
            calculateTop20(top20Map, voteList);

            List<Top20> top20s = top20Map.values()
                    .stream()
                    .sorted((o1, o2) -> o1.getWr() < o2.getWr() ? 1 : -1)
                    .collect(Collectors.toList());
            int size = top20Map.size();
            if (size == 0) {
                return;
            }
            int end = size >= 5 ? (size >= 10 ? (Math.min(size, 20)) : 0) : size;
            top20Repository.deleteAll().doOnNext(unused -> log.info("deleted top20 list")).subscribe();
            top20Repository.saveAll(top20s.subList(0, end))
                    .doOnError(throwable -> log.error("top20 error:" + throwable.getMessage()))
                    .subscribe();
        });
    }

    private void calculateTop20(Map<String, Top20> top20Map, List<Vote> voteList) {
        voteList.forEach(vote -> {
            if (!top20Map.containsKey(vote.getBookId())) {
                Optional<Book> optionalBook = bookService.findByIdAndActiveTrueAndIsPublishedTrue(vote.getBookId()).blockOptional();
                optionalBook.ifPresent(book -> {
                    float count = voteList.stream().filter(vote1 -> vote1.getBookId().equals(vote.getBookId())).count();
                    if (count < 100) {
                        return;
                    }
                    float sum = voteList.stream().filter(vote1 -> vote1.getBookId().equals(vote.getBookId())).mapToInt(Vote::getVote).sum();
                    float average = sum / count;
                    float wr = (count / (count + 100)) * (sum / count) + ((100 / count + 100) * sum / count);
                    Top20 top20 = new Top20();
                    top20.setBook(mapper.map(book, BookSoft.class));
                    top20.setWr(wr);
                    top20.setAverage(average);
                    top20.setTotalVoteCount(count);
                    top20Map.put(vote.getBookId(), top20);
                });
            }
        });
    }
}
