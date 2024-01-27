/**
 * Created by Asma on 11/03/16.
 */
(function () {
    'use strict';
    var myApp = angular.module('app');
    myApp.service('fileUploadService', function ($http, $q) {

        this.uploadFileToUrl = function (file, uploadUrl) {
            var fileFormData = new FormData();
            fileFormData.append('file', file);

            var deffered = $q.defer();
            $http.post(uploadUrl, fileFormData, {
                transformRequest: angular.identity,
                headers: {'Content-Type': undefined}

            }).success(function (response) {
                deffered.resolve(response);

            }).error(function (response) {
                deffered.reject(response);
            });

            return deffered.promise;
        }
    });
})();