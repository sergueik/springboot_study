package com.bookportal.api.entity;

import com.bookportal.api.model.enums.EnvironmentEnum;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.mongodb.core.mapping.Document;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Document
public class Version extends BaseEntity {
    private String marketVersion;
    private String minVersion;
    private EnvironmentEnum environment;
}
