'use strict';
angular.module('myApp').controller('CreditController', ['$scope', 'CreditService', '$location', function($scope, CreditService, $location) {
    var self = this;
    self.user = {
      id: null,
      firstname: '',
      lastname: '',
      dateofbirth: '',
      ssn: '',
      score: ''
    };
    self.score = '';
    self.submit = submit;
    self.reset = reset;
    self.contextpath = $location.protocol() + '://' + $location.host() + ':' + $location.port() + $location.path();
    console.log('contextpath: ' + self.contextpath);

    function creditScore(user, contextpath) {
      CreditService.creditScore(user, contextpath)
        .then(
          function(d) {
            self.user.score = d.score;
          },
          function(errResponse) {
            console.error('Error while getting credit score');
          }
        );
    }

    function submit() {
      console.log('Getting ', self.user);
      creditScore(self.user, self.contextpath);
      //reset();
    }

    function reset() {
      self.user = {
        id: null,
        firstname: '',
        lastname: '',
        dateofbirth: '',
        ssn: '',
        score: ''
      };
      $scope.myForm.$setPristine(); //reset Form
    }
  }])
  .config(function($locationProvider) {
    $locationProvider.html5Mode(true);
  });