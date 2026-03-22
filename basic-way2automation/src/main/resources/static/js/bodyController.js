(function() {
  "use strict";
  angular.module('BankApp').controller('bodyCtrl',bodyCtrl);

function bodyCtrl ($scope,$state,mockLoader) {  
    mockLoader.loadData();
    $state.transitionTo('main.options');
}
bodyCtrl.$inject = ['$scope','$state','mockLoader'];

})();