(function() {
  "use strict";
  angular.module('BankApp').controller('withdrawlCtrl',withdrawlCtrl);

            function withdrawlCtrl($scope,$timeout,Account,Transaction,CustomerData) {
                $scope.$parent.logout = true;
                $scope.amount = "";
                $scope.withdrawl = function() {
                    var txObj = Transaction.withdrawl(CustomerData.getUser().id,CustomerData.getAccount().accountNo,$scope.amount);
                    if (!txObj.success) {
                        $scope.message = "Transaction Failed. You can not withdraw amount more than the balance.";
                    } else {
                        $scope.message = "Transaction successful";
                        $scope.$emit('amountChg');
                    } 
                    $scope.amount = "";
                    $timeout(function() {
                        Account.saveObj();
                        Transaction.saveObj();
                    }, 0);
                }
            }

withdrawlCtrl.$inject = ['$scope','$timeout','Account','Transaction','CustomerData'];

})();