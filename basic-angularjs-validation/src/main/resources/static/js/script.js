var app = angular.module('developapp', []);

app.controller('formcontroller', function($scope, $http) {
    $scope.url = 'http://192.168.0.25:8080/json'; 
    $scope.validationStatus = false;
    $scope.update = function() {
        console.log($scope.validationStatus);
    }

    $scope.sendRequest = function() {
        var value = $scope.probe;
        console.log('sendRequest: ' + value);
        $http.get($scope.url + '/' + value).then(function(response) {
            // console.log('sendRequest response: '+ JSON.stringify(response));
            var result = parseInt(response.data.name, 10);
            console.log('sendRequest result: ' + result);
            if (result >= 18) {
                console.log('valid: ' + result);
                // NOTE: failing to set validity this way                       
                // $scope.form2.probe.$setValidity('charE', true);
                $scope.validationStatus = true;
            } else {
                console.log('invalid: ' + result);
                $scope.validationStatus = false;
                // $scope.form2.probe.$setValidity('charE', false);
            }
        }, function(reason) {
            console.log(reason);
        });
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

app.directive('probeValidationDirective', function() {
    return {
        require: 'ngModel',
        link: function(scope, element, attr, mCtrl) {
            function ageValidation(value) {
                if (scope.validationStatus) {
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
