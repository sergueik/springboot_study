package com.bookportal.api.entity;

import com.bookportal.api.configs.CachingConfig;
import com.bookportal.api.entity.softmodels.UserSoft;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.Date;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Document
public class EmailConfirm extends BaseEntity {
    private UserSoft user;
    private String secretKey;
    private Date validUntil = new Date(new Date().getTime() + CachingConfig.ONE_HOUR * 36);
}
