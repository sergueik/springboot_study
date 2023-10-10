var app = angular.module('Application', []);
var protocol = window.location.protocol;
// NOTE: listController and tableConteoller are polling the backend,selectController is not
var listController = function ($scope, $http, $timeout) {
    // check schema to avoid the cors error
    // access to XMLHttpRequest at 'file:///C:/cgi-bin/list.cgi' from origin 'null' has been blocked by CORS policy: 
    // Cross origin requests are only supported for protocol schemes: 
    // http, data, chrome, chrome-extension, chrome-untrusted, https
    if (protocol === 'file:') {
        data = {
            'results': [{
                'text': 'mockup data'
            }]
        };
        $scope.list_rows = data.results;

    } else {

        $scope.data = [];
        (function getListData() {
            $http({
                method: 'GET',
                url: '/cgi-bin/list.cgi'
            }).success(function (data) {
                console.log('getting list data');
                try {
                    console.log('getListData raw data:'); 
                    console.log(data);
                    $scope.list_rows = data.results;
                } catch (e) {
                    data = {
                        'results': [{
                            'text': 'mockup data'
                        }]
                    };
                    $scope.list_rows = data.results;
                }
                console.log(data.results);
                $timeout(getListData, 1000);
            });
        })();
    }
};

app.controller('ListController', listController);

var tableController = function ($scope, $http, $timeout) {
    if (protocol === 'file:') {
        data = {
            'results': [
            {
                'column1': 'server1 column 1',
                'column2': 'server1 column 2',
                'column3': 'server1 column 3',
            },
            {
                'column1': 'server1 column 1',
                'column2': 'server1 column 2',
                'column3': 'server1 column 3',
            },
            {
                'column1': 'server1 column 1',
                'column2': 'server1 column 2',
                'column3': 'server1 column 3',
            }]

        };
        $scope.table_rows = data.results;

    } else {
        $scope.data = [];
        (function getTableData() {
            $http({
                method: 'GET',
                url: 'cgi-bin/table.cgi'
            }).success(function (data) {
                console.log('getting table data');
                try {
                    console.log('getTableData raw data:'); 
                    console.log(data);
                    $scope.table_rows = data.results;
                } catch (e) {
                    data = {
                        'results': [{
                            'column1': 'server1 column 1',
                            'column2': 'server1 column 2',
                            'column3': 'server1 column 2',
                        }]
                    };
                    $scope.table_rows = data.results;
                }
                console.log(data.results);
                $timeout(getTableData, 1000);
            });
        })();
    }
};

app.controller('TableController', tableController);
app.controller('SelectController', function ($scope, $http) {
    if (protocol !== 'file:') {

        $http({
            method: 'GET',
            url: '/cgi-bin/select.cgi'
        }).success(function (data) {
            try {
                $scope.names = data;
            } catch (e) {
                $scope.names = ['mockup'];
            }
        });
    }
});


// slightly different syntax 
// https://qna.habr.com/q/1306110?e=14002190#clarification_1790048
app.controller('TableController', function ($scope, $http) {
    $http.get('cgi-bin/table.cgi')
    .then(function success(response){
      console.log('anonymous raw data: '); 
      console.log( response.data);
      $scope.table_rows = response.data.results;
    }, function error(response){
      console.log('error(2): ' + response)
    });
  });
