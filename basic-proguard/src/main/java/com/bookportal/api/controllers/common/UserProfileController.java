package com.bookportal.api.controllers.common;

import com.bookportal.api.model.PasswordUpdateDTO;
import com.bookportal.api.response.CustomResponse;
import com.bookportal.api.service.FavouriteService;
import com.bookportal.api.service.QuoteService;
import com.bookportal.api.service.UserService;
import com.bookportal.api.service.VoteService;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/v1/userProfile")
@RequiredArgsConstructor
public class UserProfileController {
    private final FavouriteService favouriteService;
    private final UserService userService;
    private final QuoteService quoteService;
    private final VoteService voteService;

    @GetMapping("/myQuotes")
    @ApiOperation(value = "users favourite quotes")
    @PreAuthorize("!hasRole('ROLE_GUEST')")
    public Mono<CustomResponse> findFavouriteQuotes(
            @ApiParam(defaultValue = "0") @RequestParam(name = "page", defaultValue = "0") int page,
            @ApiParam(defaultValue = "20") @RequestParam(name = "size", defaultValue = "20") int size) {
        return favouriteService.findFavouritesByUser(page, size)
                .map(CustomResponse::responseOk);
    }

    @GetMapping("/votedBooks")
    @ApiOperation(value = "users voted books")
    @PreAuthorize("!hasRole('ROLE_GUEST')")
    public Mono<CustomResponse> findVotedBooks(
            @ApiParam(defaultValue = "0") @RequestParam(name = "page", defaultValue = "0") int page,
            @ApiParam(defaultValue = "20") @RequestParam(name = "size", defaultValue = "20") int size) {
        return voteService.findVoteByUser(page, size)
                .map(CustomResponse::responseOk);
    }

    @PostMapping("/updatePassword")
    @ApiOperation(value = "update password")
    @PreAuthorize("!hasRole('ROLE_GUEST')")
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    public Mono<CustomResponse> changeUserPassword(@RequestBody Mono<PasswordUpdateDTO> updateDTOMono) {
        return userService.updatePasswordOnRequest(updateDTOMono)
                .map(CustomResponse::responseOk);
    }
}
