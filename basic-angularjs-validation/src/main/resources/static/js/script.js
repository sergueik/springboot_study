var app = angular.module('developapp', []);

app.controller('formcontroller', function($scope, $http) {
    $scope.url = window.location.origin + '/json';
    $scope.update = function() {
        console.log('processing');
    }
});

app.directive('ageValidationDirective', function() {
    return {
        require: 'ngModel',
        link: function(scope, element, attr, controller) {
            function ageValidation(value) {
                if (value >= 18) {
                    console.log('valid: ' + value);
                    controller.$setValidity('charE', true);
                } else {
                    console.log('invalid: ' + value);
                    controller.$setValidity('charE', false);
                }
                return value;
            }
            controller.$parsers.push(ageValidation);
        }
    };
});

app.directive('probeValidationDirective', function($http, $q) {
    return {
        require: 'ngModel',
        link: function($scope, element, attrs, ngModel) {

            ngModel.$asyncValidators.userAvailable = function(modelValue, viewValue) {
                var userInput = modelValue || viewValue;
                if (userInput === undefined) {
                    return $q.reject('mising user input');
                }
                console.log('request: ' + $scope.url + '/' + userInput);
                return $http.get($scope.url + '/' + userInput).then(function(response) {
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
