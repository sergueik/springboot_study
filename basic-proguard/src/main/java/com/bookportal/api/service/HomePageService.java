package com.bookportal.api.service;

import com.bookportal.api.entity.HomePage;
import com.bookportal.api.exception.CustomAlreadyExistException;
import com.bookportal.api.exception.CustomNotFoundException;
import com.bookportal.api.model.HomePageResponseDTO;
import com.bookportal.api.model.enums.HomePageEnum;
import com.bookportal.api.repository.HomePageRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.bson.types.ObjectId;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.mongodb.core.ReactiveMongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import reactor.core.publisher.Mono;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class HomePageService {
    private final HomePageRepository homePageRepository;
    private final ReactiveMongoTemplate reactiveMongoTemplate;
    private final BookService bookService;
    private final Top20Service top20Service;

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    @CacheEvict(value = {"homePage"})
    public Mono<HomePage> save(HomePage newHomePage) {
        if (newHomePage.getType().equals(HomePageEnum.RECOMMENDED_BOOK)) {
            return Mono.just(newHomePage)
                    .zipWith(findRecBook(newHomePage.getType())
                            .switchIfEmpty(Mono.just(newHomePage)), (homePage, o) -> homePage)
                    .flatMap(homePageRepository::save);
        }
        return Mono.just(newHomePage)
                .zipWith(findByTypeAndBook(newHomePage.getType(), newHomePage.getBook().getId())
                        .switchIfEmpty(Mono.just(newHomePage)), (homePage, o) -> homePage)
                .flatMap(homePageRepository::save);
    }

    private Mono<Object> findRecBook(HomePageEnum homePageEnum) {
        if (!homePageEnum.equals(HomePageEnum.RECOMMENDED_BOOK)) {
            return Mono.empty();
        }
        Query query = new Query()
                .addCriteria(new Criteria("type").is(HomePageEnum.RECOMMENDED_BOOK));
        return reactiveMongoTemplate.findOne(query, HomePage.class)
                .flatMap(homePage -> {
                    return Mono.error(new CustomAlreadyExistException("Recommended book already exists."));
                });
    }

    private Mono<Object> findByTypeAndBook(HomePageEnum type, String id) {
        Query query = new Query()
                .addCriteria(new Criteria("type").is(type.name()))
                .addCriteria(new Criteria("book._id").is(new ObjectId(id)));
        return reactiveMongoTemplate.findOne(query, HomePage.class)
                .flatMap(homePage -> {
                    return Mono.error(new CustomAlreadyExistException("Same record already exists"));
                });
    }

    @Cacheable("homePage")
    public Mono<HomePageResponseDTO> homePageResponse() {
        return homePageRepository.findAll()
                .collectList()
                .map(this::initHomePageResponse)
                .zipWith(bookService.getLastBooks(), (homePageResponseDTO, books) -> {
                    homePageResponseDTO.setLastBook(books);
                    return homePageResponseDTO;
                })
                .zipWith(top20Service.getTop20(), (homePageResponseDTO, top20s) -> {
                    homePageResponseDTO.setTopList(top20s);
                    return homePageResponseDTO;
                });
    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    @CacheEvict(value = {"homePage"})
    public Mono<Void> delete(Mono<HomePage> homePageMono) {
        return homePageMono
                .flatMap(homePage -> homePageRepository.findByTypeAndBook_Id(homePage.getType().name(), homePage.getBook().getId()))
                .switchIfEmpty(Mono.error(new CustomNotFoundException("No data could found")))
                .flatMap(homePageRepository::delete);
    }

    private HomePageResponseDTO initHomePageResponse(List<HomePage> homePages) {
        HomePageResponseDTO dto = new HomePageResponseDTO();
        homePages.forEach(homePage -> {
            if (homePage.getType().equals(HomePageEnum.RECOMMENDED_BOOK)) {
                dto.setRecommendedBook(homePage);
            } else if (homePage.getType().equals(HomePageEnum.RECOMMENDED_LIST)) {
                dto.getRecommendedBooksList().add(homePage);
            } else if (homePage.getType().equals(HomePageEnum.EDITORS_CHOICE)) {
                dto.getEditorsChoiceList().add(homePage);
            }
        });
        return dto;
    }
}
