package com.bookportal.api.model;

import com.bookportal.api.entity.Book;
import lombok.Data;


@Data
public class BookResponseDTO {
    Book book;
    boolean willRead;
    boolean haveRead;
}
