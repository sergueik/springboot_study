import { checkUserLoggedIn, setSessionCurrentUser } from '/assets/app/js/userCheckService.js';

export class LoginController {
   constructor($rootScope, $scope, $state, loginUserService) {
      this._userName = "";
      this._userPass = "";
      
      this._rootScope = $rootScope;
      this._scope = $scope;
      this._state = $state;
      this._isLoggingIn = false;
      
      this._loginUserSvc = loginUserService;
      
      if (checkUserLoggedIn()) {
         this._state.go("index");
      }
   }
   
   get userName() {
      return this._userName;
   }
   
   set userName(val) {
      this._userName = val;
   }
   
   get userPass() {
      return this._userPass;
   }
   
   set userPass(val) {
      this._userPass = val;
   }
   
   get isLoggingIn() {
      return this._isLoggingIn;
   }
   
   set isLoggingIn(val) {
      this._isLoggingIn = val;
   }
   
   login() {
      let self = this;
      self._isLoggingIn = true;
      self._loginUserSvc.authenticateUser(self._userName, self._userPass)
         .then(function (result) {
            if (result && result.userId && result.userId.trim() !== "") {
               console.log(result);
               setSessionCurrentUser(angular.copy(result));
               setTimeout(function() {
                  self._isLoggingIn = false;
                  self._state.go("index");
               }, 2000);
            } else {
               self._isLoggingIn = false;
               self._state.go("notAuthorized");
            }
         }, function(error) {
            if (error) {
               console.log(error);
            }
            self._isLoggingIn = false;
            self._state.go("authenticationError");
         });
   }
   
   clearForm() {
      this._userName = "";
      this._userPass = "";
   }
}