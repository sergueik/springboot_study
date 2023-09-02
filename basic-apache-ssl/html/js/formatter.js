// origin: https://docs.angularjs.org/api/ng/directive/ngBindHtml:
angular.module('Application', ['ngSanitize'])
.controller('FormatController', ['$scope', function($scope) {
  $scope.message = 'Lorem <span class="error">\\</span>ipsum dolor sit amet';
}]);
