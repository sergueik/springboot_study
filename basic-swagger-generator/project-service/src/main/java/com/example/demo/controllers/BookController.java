package com.example.demo.controllers;

import com.example.demo.api.BooksApi;
import com.example.demo.models.Book;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Slf4j
@RestController
public class BookController implements BooksApi {

	@Override
    public ResponseEntity<Void> createBook(Book body) {
        log.info("Request body received: " + body);

        // Send custom header in response
        HttpHeaders headers = new HttpHeaders();
        headers.add("Custom-Header", "Custom-Value");

        return new ResponseEntity<>(headers, HttpStatus.CREATED);
    }

    @Override
    public ResponseEntity<List<Book>> getAllBooksInLibrary() {
        List<Book> books = new ArrayList<>();

        Book book1 = new Book();
        book1.setName("Harry Potter");
        book1.setAuthor("J.K. Rowling");

        Book book2 = new Book();
        book2.setName("Lord of the Rings");
        book2.setAuthor("J.R.R. Tolkien");

        // books.addAll(List.of(book1, book2));
        books.addAll(Arrays.asList(new Book[]{book1, book2}));

        // Send custom header in response
        HttpHeaders headers = new HttpHeaders();
        headers.add("Custom-Header", "Custom-Value");

        return new ResponseEntity<>(books, headers, HttpStatus.OK);
    }

}
