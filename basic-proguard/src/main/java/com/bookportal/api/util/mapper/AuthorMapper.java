package com.bookportal.api.util.mapper;

import com.bookportal.api.entity.Author;
import com.bookportal.api.model.AuthorDTO;
import com.bookportal.api.model.AuthorUpdateDTO;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

@Component
public class AuthorMapper {
    public Mono<Author> authorDTOtoAuthor(AuthorDTO dto) {
        return Mono.fromSupplier(() -> {
            Author author = new Author();
            author.setName(dto.getName());
            author.setAbout(dto.getAbout());
            author.setImageUrl(dto.getImageUrl());
            author.setActive(true);
            return author;
        });
    }

    public Author authorUpdateDTOtoAuthor(AuthorUpdateDTO dto, Author author) {
        author.setName(dto.getName());
        author.setAbout(dto.getAbout());
        author.setImageUrl(dto.getImageUrl());
        return author;
    }
}
