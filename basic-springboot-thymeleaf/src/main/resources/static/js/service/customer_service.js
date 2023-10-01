'use strict';
angular.module('myApp').factory('CreditService', ['$http', '$q', function($http, $q) {
  var REST_SERVICE_URI = '/creditscore';
  var factory = {
    creditScore: function(user, contextpath) {
      var deferred = $q.defer();
      $http.post(contextpath + REST_SERVICE_URI, user)
        .then(
          function(response) {
            deferred.resolve(response.data);
          },
          function(errResponse) {
            console.error('Error while determine credit score');
            deferred.reject(errResponse);
          }
        );
      return deferred.promise;
    },
  };
  return factory;
}]);