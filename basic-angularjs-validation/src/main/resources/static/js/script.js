var app = angular.module('developapp', []);

app.controller('testController', function ($scope, $http) {
  $http.get('http://ip.jsontest.com').success(function(data){
    // create a scope that's attached to this returned data
    console.log('Return from controller is ' + data.ip);
    $scope.mapData = data.ip;
  });
});


app.directive('ageValidationDirective', function() {
    return {
        require: 'ngModel',
        link: function(scope, element, attr, mCtrl) {
            function ageValidation(value) {
                if (value >= 18) {
                    console.log('valid: ' + value );
                    mCtrl.$setValidity('charE', true);
                } else {
                    console.log('invalid: ' + value);
                    mCtrl.$setValidity('charE', false);
                }
                return value;
            }
            mCtrl.$parsers.push(ageValidation);
        }
    };
});
// can't inject $scope in directives , 
// https://stackoverflow.com/questions/24336858/pass-http-from-controller-to-directive
// https://docs.angularjs.org/api/ng/service/$compile#-scope-
// https://stackoverflow.com/questions/22142017/angular-directive-handles-http-request
app.directive('xxxValidationDirective',[ '$http', function($http) {
    return {
        require: 'ngModel',
        link: function(scope, element, attr, mCtrl) {
            function xxxValidation(value) {
                $http.get('http://192.168.0.64:8080/json/' + value).then(function(response) {
	            // console.log('response: '+ JSON.stringify(response));
                    var result = parseInt(response.data.name,10);
	            console.log('result: '+ result);
                    if (result >= 18) {
                        console.log('valid: ' + result );
                        mCtrl.$setValidity('charE', true);
                    } else {
                    console.log('invalid: ' + result);
                        mCtrl.$setValidity('charE', false);
                    }
                }, function(reason) {
                    console.log(reason);
                });
            }
            mCtrl.$parsers.push(xxxValidation);
        }
    };
}]);
// see also: https://stackoverflow.com/questions/14363656/using-setvalidity-inside-a-controller
