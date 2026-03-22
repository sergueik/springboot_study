(function() {
  'use strict';
  angular.module('BankApp').service('Transaction',txService);
  
  function txService(Account) {

      var txArray = {};
      var vm = this;

      vm.deposit = function(userid, accountNo,amout){
        return transaction(userid,accountNo,amout,0,new Date());
      };
      vm.withdrawl = function(userid, accountNo,amout){
        return transaction(userid,accountNo,0,amout,new Date());
      };

      vm.getTransactions = function (userid,accountNo){
        if (txArray[userid]) return (txArray[userid])[accountNo];
      }
      vm.loadObject = function(obj){

        for (var x in obj) {
          if (typeof(x) !== "undefined") {
            for (var y in obj[x]) {
              if (typeof(obj[x][y]) !== "undefined") {
                for (var i=0, len = obj[x][y].length; i < len; i++) {
                  var date = (obj[x])[y][i].date;
                  if (typeof(date) === "string") (obj[x])[y][i].date = new Date(date);                
                }
              }
            }
          }
        }
        txArray = obj;
      }
      vm.saveObj = function() {
        localStorage.setItem('Transaction',JSON.stringify(txArray));
      }
      vm.deleteUser = function(userId) {
        delete txArray[userId];
      }
      vm.deleteTx = function(userId,accountNo){
        delete (txArray[userId])[accountNo];
        var success = Account.reset(accountNo);
      }
      vm.addTransaction = function(user,account,depAmt,wdAmt,date){
        transaction(user,account,depAmt,wdAmt,date);
      }
      function transaction(user,account,depAmt,wdAmt,date) {
        txArray[user] = txArray[user] || {};
        (txArray[user])[account] = (txArray[user])[account] || [];
        if(depAmt && depAmt > 0) {
          var txObj = {};
          txObj.date = date;
          txObj.amount = depAmt;
          txObj.deposit = true;
          var success = Account.updateAmount(account,depAmt);
          txObj.success = success;
          txObj.type = "Credit";
          if(success) (txArray[user])[account].push(txObj);
        }

        if(wdAmt && wdAmt > 0) {
          var txObj = {};
          txObj.date = date;
          txObj.amount = wdAmt;
          txObj.withdrawl = true;
          var success = Account.updateAmount(account,(wdAmt * -1));
          txObj.success = success;
          txObj.type = "Debit";
          if(success) (txArray[user])[account].push(txObj);
        }
        return txObj;
      }

  }


txService.$inject = ['Account'];

})();