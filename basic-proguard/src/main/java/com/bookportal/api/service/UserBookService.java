package com.bookportal.api.service;

import com.bookportal.api.entity.Book;
import com.bookportal.api.entity.User;
import com.bookportal.api.entity.UserBook;
import com.bookportal.api.entity.softmodels.BookSoft;
import com.bookportal.api.entity.softmodels.UserSoft;
import com.bookportal.api.exception.CustomNotFoundException;
import com.bookportal.api.model.enums.ExceptionItemsEnum;
import com.bookportal.api.model.enums.UserBookEnum;
import com.bookportal.api.repository.UserBookRepository;
import lombok.RequiredArgsConstructor;
import org.bson.types.ObjectId;
import org.modelmapper.ModelMapper;
import org.springframework.data.mongodb.core.ReactiveMongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.Map;

@Service
@RequiredArgsConstructor
public class UserBookService {
    private final ReactiveMongoTemplate reactiveMongoTemplate;
    private final UserBookRepository userBookRepository;
    private final UserService userService;
    private final BookService bookService;
    private final ModelMapper modelMapper = new ModelMapper();

    public Mono<UserBook> save(String bookId, String enumId) {
        return getOrEmpty(bookId, getEnum(String.valueOf(enumId)))
                .map(userBook -> {
                    userBook.setActive(!userBook.isActive());
                    return userBook;
                })
                .switchIfEmpty(initUserBookObj(bookId,enumId))
                .flatMap(userBookRepository::save);
    }

    public Mono<Map<String, Boolean>> findTypesForBook(String bookId) {
        Map<String, Boolean> map = new HashMap<>();
        return getUser().doOnNext(user -> {
            Mono<UserBook> willReadMono = Mono.empty();//todo userBookRepository.findByUserSoft_IdAndBookSoft_IdAndType(user.getId(), bookId, UserBookEnum.WILL_READ);
            Mono<UserBook> haveReadMono = Mono.empty();//todo userBookRepository.findByUserSoft_IdAndBookSoft_IdAndType(user.getId(), bookId, UserBookEnum.HAVE_READ);
            willReadMono.zipWith(haveReadMono, (will, have) -> {
                if (will.isActive()) {
                    map.put(UserBookEnum.WILL_READ.getValue(), true);
                }
                if (have.isActive()) {
                    map.put(UserBookEnum.HAVE_READ.getValue(), true);
                }
                return map;
            });
        }).map(user -> map);
    }

    private Mono<UserBook> getOrEmpty(String bookId, UserBookEnum userBookEnum) {
        return getUser()
                .flatMap(user -> {
                    Query query = new Query()
                            .addCriteria(new Criteria("book._id").is(new ObjectId(bookId)))
                            .addCriteria(new Criteria("type").is(userBookEnum))
                            .addCriteria(new Criteria("user._id").is(new ObjectId(user.getId())));
                    return reactiveMongoTemplate.findOne(query, UserBook.class);
                }).switchIfEmpty(Mono.empty());
    }

    private Mono<UserBook> initUserBookObj(String bookId, String enumId) {
        Mono<Book> bookMono = getBook(bookId);
        Mono<User> userMono = getUser();

        return bookMono.zipWith(userMono, (book, user) -> {
            UserBook userBook = new UserBook();
            userBook.setActive(true);
            userBook.setType(UserBookEnum.findByValue(enumId));
            userBook.setBook(modelMapper.map(book, BookSoft.class));
            userBook.setUser(modelMapper.map(user, UserSoft.class));
            return userBook;
        });
    }

    private UserBookEnum getEnum(String type) {
        boolean exist = UserBookEnum.isExist(String.valueOf(type));
        if (exist) {
            return UserBookEnum.findByValue(type);
        }
        throw new CustomNotFoundException(ExceptionItemsEnum.TYPE.getValue());
    }

    private Mono<User> getUser() {
        return userService.getCurrentUser()
                .switchIfEmpty(Mono.error(new CustomNotFoundException(ExceptionItemsEnum.USER.getValue())));
    }

    private Mono<Book> getBook(String id) {
        return bookService.findByIdAndActiveTrueAndIsPublishedTrue(id);
    }
}
