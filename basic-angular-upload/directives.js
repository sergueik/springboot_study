/**
 * Created by Asma on 11/03/16.
 */
(function() {
    'use strict';
    var myApp = angular.module('app');

    /*
    A directive to enable two way binding of file field
     */
    myApp.directive('demoFileModel', function ($parse) {
        return {
            restrict: 'A', //the directive can be used as an attribute only

            /*
            link is a function that defines functionality of directive
            scope: scope associated with the element
            element: element on which this directive used
            attrs: key value pair of element attributes
             */
            link: function (scope, element, attrs) {
                var model = $parse(attrs.demoFileModel),
                    modelSetter = model.assign; //define a setter for demoFileModel

                //Bind change event on the element
                element.bind('change', function () {
                    //Call apply on scope, it checks for value changes and reflect them on UI
                    scope.$apply(function () {
                        //set the model value
                        modelSetter(scope, element[0].files[0]);
                    });
                });
            }
        };
    });
})();