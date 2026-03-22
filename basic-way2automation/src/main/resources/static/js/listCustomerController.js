(function() {
  "use strict";
  angular.module('BankApp').controller('listCustomerCtrl',listCustomerCtrl);

            function listCustomerCtrl($scope,User,Account,Transaction) {
                $scope.$parent.logout = false;
                $scope.Customers = User.getUsers();
                $scope.$parent.btnClass1 = '';
                $scope.$parent.btnClass2 = '';
                $scope.$parent.btnClass3 = 'btn-primary';
                $scope.deleteCust = function(cust) {
                    Account.deleteUser(cust.id);
                    Transaction.deleteUser(cust.id);
                    User.deleteUser(cust.id);
                    
                    $scope.Customers = User.getUsers();
                    User.saveObj();
                    Account.saveObj();
                    Transaction.saveObj();
                    
                }
            }

listCustomerCtrl.$inject = ['$scope','User','Account','Transaction'];

})();