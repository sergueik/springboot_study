package com.bookportal.api.model;

import com.bookportal.api.entity.Book;
import com.bookportal.api.entity.HomePage;
import com.bookportal.api.entity.Top20;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
public class HomePageResponseDTO {
    private List<HomePage> recommendedBooksList = new ArrayList<>();
    private List<HomePage> editorsChoiceList = new ArrayList<>();
    private List<Top20> topList = new ArrayList<>();
    private List<Book> lastBook = new ArrayList<>();
    private HomePage recommendedBook;
}
