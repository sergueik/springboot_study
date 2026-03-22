(function() {
  'use strict';
  angular.module('BankApp').service('Account',acctService);
  
  function acctService(User) {

      var acctArray = {};
      var vm = this;
      var availableAcctNo = 1000;

      vm.createAccount = function(userId, currency){
        if(userId && currency) {
          availableAcctNo++;
          var account = {};
          account.accountNo = availableAcctNo;
          account.currency = currency;
          account.userId = userId;
          account.date = new Date();
          account.amount = 0;
          acctArray[availableAcctNo] = account;
          User.addAccount(userId,availableAcctNo);
          return availableAcctNo;
        }
        return 0;
      };

      vm.getAccount = function(accountNo) {
        return acctArray[accountNo];
      }
      
      vm.updateAmount = function (accountNo,amount){
        acctArray[accountNo].amount = acctArray[accountNo].amount || 0;
        if (amount < 0 && (acctArray[accountNo].amount + amount) < 0) {
          console.error("Can not perform this transaction");
          return false;
        } else {
          acctArray[accountNo].amount = acctArray[accountNo].amount + amount; 
          return true;         
        }
      }
      vm.reset = function(accountNo) {
        acctArray[accountNo].amount = 0;
      }
      vm.loadObject = function(obj){
        availableAcctNo = localStorage.getItem('maxAccountNo');
        acctArray = obj;
      }
      vm.saveObj = function() {
        localStorage.setItem('Account',JSON.stringify(acctArray));
        localStorage.setItem('maxAccountNo',availableAcctNo);
      }
      vm.deleteAccount = function(accountNo) {
        delete acctArray[accountNo];
      } 
      vm.deleteUser = function(userId) {
        var accounts = User.getUserAccounts(userId);
        if (accounts) {
          for (var i=0, len = accounts.length; i<len; i++){
            delete acctArray[accounts[i]]; 
          }
        }
      }
  }

acctService.$inject = ['User'];

})();