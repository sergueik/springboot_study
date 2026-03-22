import { getUserSecurityToken } from '/assets/app/js/userCheckService.js';

export function bookKeepingService($resource) {
   let retVal = { };
 
   retVal.getAllBooks = function () {
      let authToken = getUserSecurityToken();
      let apiRes = $resource(null, null, {
         getAllBooks: {
            url: "/secure/api/allBooks",
            method: "get",
            headers: {
               "authorization": "bearer " + authToken
            },
            isArray: true
         }
      });
      
      return apiRes.getAllBooks().$promise;
   };

   retVal.addNewBook = function (bookToAdd) {
      let authToken = getUserSecurityToken();
      let apiRes = $resource(null, null, {
         addNewBook: {
            url: "/secure/api/newBook",
            method: "post",
            headers: {
               "authorization": "bearer " + authToken
            },
            isArray: false
         }
      });
      
      return apiRes.addNewBook(bookToAdd).$promise;
   };

   retVal.deleteBook = function (isbnCode) {
      let authToken = getUserSecurityToken();
      let apiRes = $resource(null, null, {
         deleteBook: {
            url: "/secure/api/deleteBook",
            method: "delete",
            headers: {
               "authorization": "bearer " + authToken
            },
            isArray: false,
            params: {
               title: "@title"
            }
         }
      });
      
      return apiRes.deleteBook({ isbn: isbnCode }).$promise;
   };
      
   return retVal;
}