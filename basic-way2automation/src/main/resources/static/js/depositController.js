(function() {
  "use strict";
  angular.module('BankApp').controller('depositCtrl',depositCtrl);

            function depositCtrl($scope,$timeout,Account,Transaction,CustomerData) {
                $scope.$parent.logout = true;
                $scope.amount = "";
                $scope.deposit = function() {
                    var txObj = Transaction.deposit(CustomerData.getUser().id,CustomerData.getAccount().accountNo,$scope.amount);
                    if (txObj.success) {
                        $scope.message = "Deposit Successful";
                        $scope.$emit('amountChg');
                    } else {
                        $scope.message = "Something went wrong. Please try again.";
                    }
                    $scope.amount = "";
                    $timeout(function() {
                        Account.saveObj();
                        Transaction.saveObj();
                    }, 0);
                }
            }
depositCtrl.$inject = ['$scope','$timeout','Account','Transaction','CustomerData'];

})();