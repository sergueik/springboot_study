import { checkUserLoggedIn, removeSessionCurrentUser } from '/assets/app/js/userCheckService.js';

export class IndexController {
   constructor($rootScope, $scope, $state, bookKeepingService) {
      this._rootScope = $rootScope;
      this._scope = $scope;
      this._state = $state;
      this._errorMsg = null;
      this._bookKeepingSvc = bookKeepingService;
      
      this._foundBooks = null;
      this._bookTitle = null;
      this._bookPublishedYear = 2001;
      this._isbnCode = null;
      
      if (!checkUserLoggedIn()) {
         this._state.go("login");
      }
      
      this.loadAllBooks();
   }

   get foundBooks() {
      return this._foundBooks;    
   }
   
   set foundBooks(val) {
      this._foundBooks = val;
   }

   get errorMsg() {
      return this._errorMsg;    
   }
   
   set errorMsg(val) {
      this._errorMsg = val;
   }
   
   get bookTitle() {
      return this._bookTitle;
   }
   
   set bookTitle(val) {
      this._bookTitle = val;
   }
   
   get bookPublishedYear() {
      return this._bookPublishedYear;
   }
   
   set bookPublishedYear(val) {
      this._bookPublishedYear = val;
   }
   
   get isbnCode() {
      return this._isbnCode;
   }
   
   set isbnCode(val) {
      this._isbnCode = val;
   }
   
   loadAllBooks() {
      let self = this;
      self._errorMsg = null;
      self._bookKeepingSvc.getAllBooks()
         .then(function (results) {
            if (results) {
               if (results.length > 0) {
                  self._foundBooks = results;
               } else {
                  self._errorMsg = "No books list available.";
               }
            } else {
               self._errorMsg = "No books list available.";
            }
         }, function(error) {
            if (error) {
               console.log(error);
               if (error.status == 401 || error.status == 403) {
                  removeSessionCurrentUser();
                  self._state.go("login");
               } else {
                  self._errorMsg = "Unable to load books list. Server error.";
               }
            }
         });
   }
   
   clickDeleteBook(bookToDelete) {
      if (bookToDelete) {
         let self = this;
         self._errorMsg = null;
         self._bookKeepingSvc.deleteBook(bookToDelete.title)
            .then(function (result) {
               if (result) {
                  if (result.successful) {
                     let newBookList = [];
                     angular.forEach(self._foundBooks, function(book) {
                        if (book != null && book.isbnCode !== bookToDelete.isbnCode) {
                           newBookList.push(book);
                        }
                     });
                     self._foundBooks = newBookList;
                  } else {
                     self._errorMsg = "Unable to delete book from list. Unknown error.";
                  }
               } else {
                  self._errorMsg = "Unable to delete book from list. Unknown error.";
               }
            }, function (error) {
               if (error) {
                  console.log(error);
                  if (error.status == 401) {
                     removeSessionCurrentUser();
                     self._state.go("login");
                  } else if (error.status == 403) {
                     self._errorMsg = "You are not authorized to do this.";
                  } else {
                     self._errorMsg = "Unable to delete book from list. Unknown error.";
                  }
               } else {
                  self._errorMsg = "Unable to delete book from list. Unknown error.";
               }
            });
      }
   }
   
   logout() {
      let self = this;
      removeSessionCurrentUser();
      self._state.go("login");
   }
   
   addBook() {
      this._errorMsg = null;
      if (this._bookTitle != null && this._bookTitle.length > 0 &&
         this._bookPublishedYear > 2001 &&
         this._isbnCode != null && this._isbnCode.length > 0) {
         
         let bookToAdd =  {
            title: this._bookTitle,
            isbnCode: this._isbnCode,
            yearPublished: this._bookPublishedYear
         };
         
         let self = this;
         this._bookKeepingSvc.addNewBook(bookToAdd)
            .then(function (result) {
               if (result) {
                  if (result.successful) {
                     if (self._foundBooks == null) {
                        self._foundBooks = [];
                     }
                     self._foundBooks.push(bookToAdd);
                     self.clearInputs();
                  } else {
                     self._errorMsg = "Unable to delete book from list. Unknown error.";
                  }
               } else {
                  self._errorMsg = "Unable to delete book from list. Unknown error.";
               }$rootScope
            }, function (error) {
               if (error) {
                  console.log(error);
                  if (error.status === 401) {
                     removeSessionCurrentUser();
                     self._state.go("login");
                  } else if (error.status === 403) {
                     self._errorMsg = "You are not authorized to do this.";
                  } else {
                     self._errorMsg = "Unable to add new book from list. Unknown error.";
                  }
               } else {
                  self._errorMsg = "Unable to add new book from list. Unknown error.";
               }
            });
      }
   }
   
   clearInputs() {
      this._bookTitle = null;
      this._bookPublishedYear = 2001;
      this._isbnCode = null;
   }
}