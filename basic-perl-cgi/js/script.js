var app = angular.module('Application', []);

var listController = function ($scope, $http, $timeout) {
    $scope.data = [];
    (function getData() {
        $http({
        method: 'GET',
        url: '/cgi-bin/list.cgi'
      }).success(function (data) {
            console.log('getting data');
            try {
                console.log('got raw data: ' + data);
                $scope.rows = data.results;
            } catch (e) {
                data = {
                    'results': [{
                        'text': 'mockup data'
                    }]
                };
            }
            console.log(data.results);
            $timeout(getData, 1000);
        });
    })();
};


app.controller('ListController', listController);

app.controller('TableController', function($scope, $http) {
    $http({
        method: 'GET',
        url: 'cgi-bin/table.cgi'
    }).success(
        function(data) {
            console.log('getting data');
            try {
                console.log('got raw data: ' + data);
                $scope.rows = data.results;
            } catch (e) {
                data = {
                    'results': [{
                        'text': 'mockup data'
                    }]
                };
            }
            console.log('got from /table:' + data.results);
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



