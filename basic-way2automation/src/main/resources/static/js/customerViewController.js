(function() {
  "use strict";
  angular.module('BankApp').controller('customerViewCtrl',customerViewCtrl);

            function customerViewCtrl($scope,$state,User,Account,CustomerData) {
                $scope.$parent.logout = false;
                $scope.custId = '';
                $scope.Customers = User.getUsers();
                $scope.showAccount = function() {
                    CustomerData.setUser(User.getUser($scope.custId));
                    CustomerData.setAccount();
                    $state.transitionTo('main.account');
                }

            }
customerViewCtrl.$inject = ['$scope','$state','User','Account','CustomerData'];

})();