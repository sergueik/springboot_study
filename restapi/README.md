### Info

This directory contains a skeleton springboot restapi project based on
[springboot-rest-api-demo](https://github.com/bharat0126/springboot-rest-api-demo)


### Usage

Run test locally

```sh
mvn spring-boot:run
```
* exercise validations
```sh
curl -XPOST http://localhost:8989/book/add -d "{}"  -H "Content-type:application/json" 2>nul | jq '.'
```
will throw error
```json
{
  "timestamp": 1612028363988,
  "status": 400,
  "error": "Bad Request",
  "exception": "org.springframework.web.bind.MethodArgumentNotValidException",
  "message": "Validation failed for argument at index 0 in method: public java.util.Map<java.lang.String, java.lang.Object> app.controller.BookController.addBook(app.model.Book), with 2 error(s): [Field error in object 'book' on field 'isbn': rejected value [null]; codes [NotNull.book.isbn,NotNull.isbn,NotNull.java.lang.String,NotNull]; arguments [org.springframework.context.support.DefaultMessageSourceResolvable:
   codes [book.isbn,isbn]; arguments []; default message [isbn]];
   default message [idbn is missing]]
   [Field error in object 'book' on field 'isbn': rejected value [null];
   codes [NotEmpty.book.isbn,NotEmpty.isbn,NotEmpty.java.lang.String,NotEmpty];
   arguments [org.springframework.context.support.DefaultMessageSourceResolvable:
   codes [book.isbn,isbn]; arguments []; default message [isbn]]; default message [idbn is empty]] ",
  "path": "/book/add"
}
```
the message schema variues with version of Spring framework. it can become

```json
{
  "timestamp": 1612029070201,
  "status": 400,
  "error": "Bad Request",
  "exception": "org.springframework.web.bind.MethodArgumentNotValidException",
  "errors": [
    {
      "codes": [
        "NotNull.book.isbn",
        "NotNull.isbn",
        "NotNull.java.lang.String",
        "NotNull"
      ],
      "arguments": [
        {
          "codes": [
            "book.isbn",
            "isbn"
          ],
          "arguments": null,
          "defaultMessage": "isbn",
          "code": "isbn"
        }
      ],
      "defaultMessage": "idbn is missing",
      "objectName": "book",
      "field": "isbn",
      "rejectedValue": null,
      "bindingFailure": false,
      "code": "NotNull"
    },
    {
      "codes": [
        "NotEmpty.book.isbn",
        "NotEmpty.isbn",
        "NotEmpty.java.lang.String",
        "NotEmpty"
      ],
      "arguments": [
        {
          "codes": [
            "book.isbn",
            "isbn"
          ],
          "arguments": null,
          "defaultMessage": "isbn",
          "code": "isbn"
        }
      ],
      "defaultMessage": "idbn is empty",
      "objectName": "book",
      "field": "isbn",
      "rejectedValue": null,
      "bindingFailure": false,
      "code": "NotEmpty"
    }
  ],
  "message": "Validation failed for object='book'. Error count: 2",
  "path": "/book/add"
}

```
```sh
curl -XPOST http://localhost:8989/book/add -d "{\"isbn\":\"\"}"  -H "Content-type:application/json"  2>nul | jq '.'
```
```sh
{
  "timestamp": 1612028414972,
  "status": 400,
  "error": "Bad Request",
  "exception": "org.springframework.web.bind.MethodArgumentNotValidException",
  "message": "Validation failed for argument at index 0 in method: public java.util.Map<java.lang.String, java.lang.Object>
  app.controller.BookController.addBook(app.model.Book),
  with 1 error(s): [Field error in object 'book' on field 'isbn': rejected value [];
  codes [NotEmpty.book.isbn,NotEmpty.isbn,NotEmpty.java.lang.String,NotEmpty];
  arguments [org.springframework.context.support.DefaultMessageSourceResolvable:
  codes [book.isbn,isbn]; arguments [];
  default message [isbn]]; default message [idbn is empty]] ",
  "path": "/book/add"
}

```
* note: in the absence of mongodb locally will fail but go as far as  try to insert

```sh
curl -XPOST http://localhost:8989/book/add -d "{\"isbn\":\"12345\", \"name\":\"richard stevens\", \"pages\":1000, \"name\": \"advanced programming in unix environment\"}"  -H "Content-type:application/json"
```
```sh
{
  "timestamp": 1612028760980,
  "status": 500,
  "error": "Internal Server Error",
  "exception": "org.springframework.dao.DataAccessResourceFailureException",
  "message": "Timed out after 10000 ms while waiting for a server that matches AnyServerSelector{}.
  Client view of cluster state is {
    type=Unknown,
    servers=[{address=localhost:27017,
    type=Unknown,
    state=Connecting,
    exception={com.mongodb.MongoException$Network: Exception opening the socket
   }, caused by {java.net.ConnectException: Connection refused: connect}}]; nested exception is
   com.mongodb.MongoTimeoutException:
   Timed out after 10000 ms while waiting for a server that matches AnyServerSelector{}.
   Client view of cluster state is
   {
     type=Unknown,
     servers=[{address=localhost:27017, type=Unknown, state=Connecting, exception={com.mongodb.MongoException$Network:
   Exception opening the socket}, caused by {java.net.ConnectException: Connection refused: connect}}]",
  "path": "/book/add"
}

```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
