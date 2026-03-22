(function() {
  'use strict';
  angular.module('BankApp').service('CustomerData',customerData);
  
  function customerData($timeout) {

      var vm = this;
      var customer;
      var account;

    	vm.setUser = function(user){
        customer = user;
        $timeout(function() {
          localStorage.setItem('CurrentUser',JSON.stringify(customer));
        }, 0);
    	};

      vm.getUser = function() {
        if (typeof(customer) === "undefined") {
          var obj =localStorage.getItem('CurrentUser');
          if (obj) customer = JSON.parse(obj);
        }
        return customer;
      }

      vm.setAccount = function(acct){
        account = acct;
        if (typeof(account) !== "undefined") localStorage.setItem('CurrentAccount',JSON.stringify(account));
          else localStorage.removeItem('CurrentAccount');
      };

      vm.getAccount = function() {
        if(typeof(account) === "undefined") {
          var obj =localStorage.getItem('CurrentAccount');
          if (obj) account = JSON.parse(obj);
        }
         return account;
      }

  }
  customerData.$inject = ['$timeout'];

})();