(function() {
  "use strict";
  angular.module('BankApp').controller('openAccountCtrl',openAccountCtrl);

            function openAccountCtrl($scope,$timeout,Account,User){
                $scope.Customers = User.getUsers();
                $scope.$parent.btnClass1 = '';
                $scope.$parent.btnClass2 = 'btn-primary';
                $scope.$parent.btnClass3 = '';

                $scope.$parent.logout = false;
                $scope.process = function() {
                    var acctNo = Account.createAccount($scope.custId,$scope.currency);
                    if (acctNo == 0) {
                        alert("Something went wrong. Account can not be opened.");
                    } else {
                        alert("Account created successfully with account Number :"+acctNo);
                        $scope.currency = "";
                        $scope.custId = "";
                        $timeout(function() {
                            User.saveObj();
                            Account.saveObj();
                        }, 0);
                    }
                }

            }

openAccountCtrl.$inject = ['$scope','$timeout','Account','User'];

})();