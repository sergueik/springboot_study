package org.hanbo.boot.app.controllers;

import java.util.ArrayList;
import java.util.List;

import org.hanbo.boot.app.models.BookModel;
import org.hanbo.boot.app.models.OpResponse;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class SecureApiController {
	public SecureApiController() {

	}

	@PreAuthorize("hasRole('USER')")
	@RequestMapping(value = "/secure/api/allBooks", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<List<BookModel>> getAllBooks() {
		List<BookModel> resp = new ArrayList<BookModel>();

		BookModel bookToAdd = new BookModel();
		bookToAdd.setIsbnCode("9780593497333");
		bookToAdd.setTitle("Go Tell the Bees That I Am Gone");
		bookToAdd.setYearPublished(2021);
		resp.add(bookToAdd);

		bookToAdd = new BookModel();
		bookToAdd.setIsbnCode("9780316485647");
		bookToAdd.setTitle("The Dark Hours");
		bookToAdd.setYearPublished(2019);
		resp.add(bookToAdd);

		bookToAdd = new BookModel();
		bookToAdd.setIsbnCode("9781982154875");
		bookToAdd.setTitle("Game On: Tempting Twenty-Eight");
		bookToAdd.setYearPublished(2016);
		resp.add(bookToAdd);

		ResponseEntity<List<BookModel>> retVal = ResponseEntity.ok(resp);
		return retVal;
	}

	@PreAuthorize("hasRole('STAFF')")
	@RequestMapping(value = "/secure/api/newBook", method = RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<OpResponse> addNewBooks(@RequestBody BookModel bookToAdd) {
		if (bookToAdd != null) {
			OpResponse resp = new OpResponse();
			resp.setSuccessful(true);
			resp.setStatus("Successfully Add New Book");
			resp.setDetailMessage(
					String.format("Adding new book \"%s\" completed successfully.", bookToAdd.getTitle()));

			return ResponseEntity.ok(resp);
		} else {
			OpResponse resp = new OpResponse();
			resp.setSuccessful(false);
			resp.setStatus("Unable to add new book");
			resp.setDetailMessage("There is no valid new book data model to be added.");

			return ResponseEntity.ok(resp);
		}
	}

	@PreAuthorize("hasRole('ADMIN')")
	@RequestMapping(value = "/secure/api/deleteBook", method = RequestMethod.DELETE)
	public ResponseEntity<OpResponse> deleteBook(@RequestParam("isbn") String isbnCode) {
		if (!StringUtils.isEmpty(isbnCode)) {
			System.out.println("Received ISBN Code for book to delete: " + isbnCode);
			OpResponse resp = new OpResponse();
			resp.setSuccessful(true);
			resp.setStatus("Successfully Deleted Specified Book");
			resp.setDetailMessage(
					String.format("Deleting book (with ISBN code: \"%s\") completed successfully.", isbnCode));

			return ResponseEntity.ok(resp);
		} else {
			System.out.println("Delete book failed. ISBN code is not valid.");
			OpResponse resp = new OpResponse();
			resp.setSuccessful(false);
			resp.setStatus("Unable to delete book");
			resp.setDetailMessage("There is no valid book title to find the book to be deleted.");

			return ResponseEntity.ok(resp);
		}
	}
}
