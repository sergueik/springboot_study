package com.bookportal.api.entity;

import com.bookportal.api.entity.softmodels.BookSoft;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Document
public class Top20 {
    @Id
    private String id;
    private BookSoft book;
    private float average;
    private float wr;
    private float totalVoteCount;
}
