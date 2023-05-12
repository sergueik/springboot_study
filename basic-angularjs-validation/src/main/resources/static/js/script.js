
var app = angular.module('developapp', []);
app.directive('ageValidationDirective', function () {
		return {
			require: 'ngModel',
			link: function (scope, element, attr, mCtrl) {
				function ageValidation(value) {
					if (value >= 18) {
						mCtrl.$setValidity('charE', true);
					} else {
						mCtrl.$setValidity('charE', false);
					}
					return value;
				}
				mCtrl.$parsers.push(ageValidation);
			}
		};
	});
