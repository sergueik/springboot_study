// <https://stackoverflow.com/questions/19408011/angularjs-error-argument-firstctrl-is-not-a-function-got-undefined/19408070
var app = angular.module('Application', []);
app.controller('ListController', function($scope, $http) {
    $http({
        method: 'GET',
        url: '/list'
    }).success(
        function(data) {
            console.log("getting data");
            try {
                console.log("got raw data: " + data);
                $scope.rows = data.results;
            } catch (e) {
                data = {
                    "results": [{
                        "text": "mockup data"
                    }]
                };
            }
            console.log(data.results);
        });
});

app.controller('TableController', function($scope, $http) {
    $http({
        method: 'GET',
        url: '/table'
    }).success(
        function(data) {
            console.log("getting data");
            try {
                console.log("got raw data: " + data);
                $scope.rows = data.results;
            } catch (e) {
                data = {
                    "results": [{
                        "text": "mockup data"
                    }]
                };
            }
            console.log("got from /table:" + data.results);
        });
});


app.controller('SelectController', function($scope, $http) {
    $http({
        method: 'GET',
        url: '/select'
    }).success(function(data) {
        try {
            $scope.names = data;
        } catch (e) {
            $scope.names = ["mockup"];
        }
    });
});

