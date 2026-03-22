(function() {
  "use strict";
  angular.module('BankApp').controller('mainCtrl',mainCtrl);

function mainCtrl ($scope,$state){
    $scope.home = function() {
         $state.transitionTo('main.options');
        }
    $scope.byebye = function() {
         $state.transitionTo('main.custView');
        }
                
}

mainCtrl.$inject = ['$scope','$state'];

})();