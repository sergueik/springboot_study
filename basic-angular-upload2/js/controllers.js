/**
 * Created by Asma on 11/03/16.
 */
(function () {
    'use strict';
    var myApp = angular.module('app');
    myApp.controller('FileUploadController', function ($scope, fileUploadService) {

        $scope.uploadFile = function () {
            var file = $scope.myFile;
            var uploadUrl = "../server/service.php", //Url of web service
                promise = fileUploadService.uploadFileToUrl(file, uploadUrl);

            promise.then(function (response) {
                $scope.serverResponse = response;
            }, function () {
                $scope.serverResponse = 'An error has occurred';
            })
        };
    });

})();