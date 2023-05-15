var app = angular.module('developapp', []);

app.controller('formcontroller', function($scope, $http) {
    $scope.url = 'http://192.168.0.25:8080/json';
    $scope.update = function() {
        console.log('processing');
    }
});

app.directive('ageValidationDirective', function() {
    return {
        require: 'ngModel',
        link: function(scope, element, attr, mCtrl) {
            function ageValidation(value) {
                if (value >= 18) {
                    console.log('valid: ' + value);
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

app.directive('probeValidationDirective', function($http, $q) {
    return {
        require: 'ngModel',
        link: function($scope, element, attrs, ngModel) {

            ngModel.$asyncValidators.userAvailable = function(modelValue, viewValue) {

                var userInput = modelValue || viewValue;
                return $http.get('http://192.168.0.25:8080/json/' + userInput).then(function(response) {
                    // console.log('response: '+ JSON.stringify(response));
                    var result = parseInt(response.data.name, 10);
                    console.log('result: ' + result);

                    if (result >= 18) {
                        console.log('valid: ' + result);
                        return true;
                    } else {
                        console.log('invalid: ' + result);

                        return $q.reject('invalid: ' + result);
                    }
                }, function(reason) {
                    console.log(reason);
                    return $q.reject('failed backend call: ' + reason);
                });
            }
        }
    }
});