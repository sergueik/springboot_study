(function() {
  'use strict';
  angular.module('BankApp').service('User',userService);
  
  function userService() {

      var userArray = {};
      var vm = this;
      var availableUserId = 0;

    	vm.addUser = function(user){

        if(user && user.fName && user.lName && user.postCd && !duplicate(user)) {
          availableUserId++;
          user.id = availableUserId;
          user.date = new Date();
          userArray[availableUserId] = user;
          return availableUserId;
        }
        return 0;
    	};

      vm.getUser = function(uniqueId) {
        return userArray[uniqueId];
      }

      vm.getUsers = function() {
        var array = [];
        for (var userid in userArray){
          array.push(userArray[userid]);
        }
        return array;
      }
      vm.deleteUser = function(uniqueId) {
        delete userArray[uniqueId];
      } 
      vm.addAccount = function(uniqueId,accountNo) {
        userArray[uniqueId].accountNo = userArray[uniqueId].accountNo || [];
        userArray[uniqueId].accountNo.push(accountNo);
      }
      vm.getUserAccounts = function(uniqueId) {
        return userArray[uniqueId].accountNo;
      }
      vm.loadObject = function(obj){
        userArray = obj;
        availableUserId = localStorage.getItem('maxUserId');
      }
      vm.saveObj = function() {
        localStorage.setItem('User',JSON.stringify(userArray));
        localStorage.setItem('maxUserId',availableUserId);
      }
      function duplicate(userObj) {
        for (var user in userArray) {
          if (userObj.fName === userArray[user].fName && userObj.lName === userArray[user].lName && userObj.postCd === userArray[user].postCd) {
            return true;
          }
        }
        return false;
      }
  }

})();