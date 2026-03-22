(function() {
  "use strict";
  angular.module('BankApp').controller('addCustomerCtrl',addCustomerCtrl);

            function addCustomerCtrl($scope,$timeout,User){
                $scope.$parent.logout = false;
                $scope.$parent.btnClass1 = 'btn-primary';
                $scope.$parent.btnClass2 = '';
                $scope.$parent.btnClass3 = '';
                $scope.addCustomer = function() {
                    var user = {};
                    user.fName = $scope.fName;
                    user.lName = $scope.lName;
                    user.postCd = $scope.postCd;
                    var id = User.addUser(user);
                    if (id === 0) {
                        alert("Please check the details. Customer may be duplicate.");
                    } else {
                        alert("Customer added successfully with customer id :"+id);
                        $scope.fName = "";
                        $scope.lName = "";
                        $scope.postCd = "";
                        $timeout(function() {
                            User.saveObj();
                        }, 0);
                    }
                }

            }

addCustomerCtrl.$inject = ['$scope','$timeout','User'];

})();