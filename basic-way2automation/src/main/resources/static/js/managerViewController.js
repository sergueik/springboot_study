(function() {
  "use strict";
  angular.module('BankApp').controller('managerViewCtrl',managerViewCtrl);

            function managerViewCtrl($scope,$state,$timeout,User,Account){
                $scope.$parent.logout = false;
                $scope.addCust = function() {
                    $state.transitionTo('main.mgrView.add');
                    
                }
                $scope.openAccount = function() {
                    $state.transitionTo('main.mgrView.account');   
                    
                }
                $scope.showCust = function() {
                    $state.transitionTo('main.mgrView.list');     
                }

            }

managerViewCtrl.$inject = ['$scope','$state','$timeout','User','Account'];

})();