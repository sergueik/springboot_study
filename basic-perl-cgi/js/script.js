var app = angular.module('Application', []);
// NOTE: listController and tableConteoller are polling the backend,selectController is not
var listController = function ($scope, $http, $timeout) {
    $scope.data = [];
    (function getListData() {
        $http({
        method: 'GET',
        url: '/cgi-bin/list.cgi'
      }).success(function (data) {
            console.log('getting list data');
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
            $timeout(getListData, 1000);
        });
    })();
};

app.controller('ListController', listController);

var tableController = function ($scope, $http, $timeout) {
    $scope.data = [];
    (function getTableData() {
        $http({
        method: 'GET',
        url: 'cgi-bin/table.cgi'
      }).success(function (data) {
            console.log('getting table data');
            try {
                console.log('got raw data: ' + data);
                $scope.rows = data.results;
            } catch (e) {
                data = {
                    'results': [{
                        'column1': 'server1 column 1',
                        'column2': 'server1 column 2',
                        'column3': 'server1 column 2',
                    }]
                };
            }
            console.log(data.results);
            $timeout(getTableData, 1000);
        });
    })();
};

app.controller('TableController', tableController);
/*
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
*/

app.controller('SelectController', function($scope, $http) {
    $http({
        method: 'GET',
        url: '/cgi-bin/select.cgi'
    }).success(function(data) {
        try {
            $scope.names = data;
        } catch (e) {
            $scope.names = ["mockup"];
        }
    });
});



