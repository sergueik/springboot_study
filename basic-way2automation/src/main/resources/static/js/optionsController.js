(function() {
  "use strict";
  angular.module('BankApp').controller('optionCtrl',optionCtrl);


            function optionCtrl ($scope,$state){
                $scope.$parent.logout = false;
                $scope.manager = function() {
                    $state.transitionTo('main.mgrView');
                }
                $scope.customer = function() {
                    $state.transitionTo('main.custView');                    
                }
            }

optionCtrl.$inject = ['$scope','$state'];

})();