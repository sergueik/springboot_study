/**
 * Created with IntelliJ IDEA.
 * User: Furkan Bayraktar
 * Date: 7/11/14
 * Time: 12:14 PM
 * To change this template use File | Settings | File Templates.
 */

'use strict';

var app = angular.module('app', ['ngRoute']);

app.config(['$routeProvider',
    function($routeProvider) {
        $routeProvider.
            when('/', {
                templateUrl: 'home.html',
                controller: 'HomeController'
            }).
            when('/about', {
                templateUrl: 'about.html',
                controller: 'AboutController'
            }).
            when('/contact', {
                templateUrl: 'contact.html',
                controller: 'ContactController'
            }).
            otherwise({redirectTo:'/'});
    }]);

app.controller('HomeController',
    function($scope, $routeParams, $http, $location) {

    }
);

app.controller('AboutController',
    function($scope, $routeParams, $http, $location) {

    }
);

app.controller('ContactController',
    function($scope, $routeParams, $http, $location) {

    }
);