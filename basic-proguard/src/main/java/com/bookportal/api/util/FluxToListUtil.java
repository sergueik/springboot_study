package com.bookportal.api.util;

import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.support.PageableExecutionUtils;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.io.Serializable;

@Slf4j
public class FluxToListUtil implements Serializable {
    public static Mono<Page<?>> toListWithPagination(Flux<?> list, Mono<Long> count, Pageable pageable) {
        return list.collectList()
                .zipWith(count, (objects, aLong) ->
                        PageableExecutionUtils.getPage(objects, pageable, () -> aLong));
    }
}
