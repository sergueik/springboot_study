package com.bookportal.api.util.mapper;

import com.bookportal.api.entity.Author;
import com.bookportal.api.entity.Book;
import com.bookportal.api.entity.Publisher;
import com.bookportal.api.entity.User;
import com.bookportal.api.entity.softmodels.BookSoft;
import com.bookportal.api.entity.softmodels.UserSoft;
import com.bookportal.api.model.BookDTO;
import com.bookportal.api.model.BookUpdateDTO;

import java.util.List;

public class BookMapper {
    public static Book bookDTOtoBook(BookDTO bookDTO, List<Author> authorList, Publisher publisher, User user) {
        UserSoft userSoft = UserSoft.builder()
                .id(user.getId())
                .name(user.getName())
                .surname(user.getSurname())
                .mail(user.getMail())
                .build();
        Book book = new Book();
        book.setName(bookDTO.getName());
        book.setAuthors(authorList);
        book.setPage(bookDTO.getPage());
        book.setPublisher(publisher);
        book.setYear(bookDTO.getYear());
        book.setImageUrl(bookDTO.getImageUrl());
        book.setUser(userSoft);
        book.setTag(bookDTO.getTag());
        return book;
    }

    public static Book bookUpdateDTOtoBook(BookUpdateDTO bookUpdateDTO, Book book, List<Author> authorList, Publisher publisher, User user) {
        UserSoft userSoft = UserSoft.builder()
                .id(user.getId())
                .name(user.getName())
                .surname(user.getSurname())
                .mail(user.getMail())
                .build();
        book.setName(bookUpdateDTO.getName());
        book.setAuthors(authorList);
        book.setPage(bookUpdateDTO.getPage());
        book.setPublisher(publisher);
        book.setYear(bookUpdateDTO.getYear());
        book.setImageUrl(bookUpdateDTO.getImageUrl());
        book.setTag(bookUpdateDTO.getTag());
        book.setEditor(userSoft);
        book.setIsPublished(bookUpdateDTO.getIsPublished());
        return book;
    }

    public static BookSoft bookToSoft(Book book){
        return BookSoft.builder()
                .id(book.getId())
                .name(book.getName())
                .build();
    }
}
