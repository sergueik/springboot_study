var app = angular.module('plunker', ['ngAnimate']);

// origin: https://blog.guya.net/2016/08/08/simple-server-polling-in-angularjs-done-right/

app.controller('MainCtrl', function($scope, $http, $timeout) {

  var loadTime = 1000, //Load the data every second
    errorCount = 0, //Counter for the server errors
    loadPromise; //Pointer to the promise created by the Angular $timout service

  var getData = function() {
    $http.get('http://httpbin.org/delay/1?now=' + Date.now())

    .then(function(res) {
      $scope.data = res.data.args;

      errorCount = 0;
      nextLoad();
    })

    .catch(function(res) {
      $scope.data = 'Server error';
      nextLoad(++errorCount * 2 * loadTime);
    });
  };

  var cancelNextLoad = function() {
    $timeout.cancel(loadPromise);
  };

  var nextLoad = function(mill) {
    mill = mill || loadTime;
    
    //Always make sure the last timeout is cleared before starting a new one
    cancelNextLoad();
    loadPromise = $timeout(getData, mill);
  };


  //Start polling the data from the server
  getData();


  //Always clear the timeout when the view is destroyed, otherwise it will keep polling
  $scope.$on('$destroy', function() {
    cancelNextLoad();
  });

  $scope.data = 'Loading...';
});
