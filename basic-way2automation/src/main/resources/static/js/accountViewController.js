(function() {
  "use strict";
  angular.module('BankApp').controller('accountCtrl',accountCtrl);

            function accountCtrl($scope,$state,CustomerData,User,Account,Transaction){
                $scope.$parent.logout = true;
                
                var userObj = CustomerData.getUser();
                $scope.user = userObj.fName + " " + userObj.lName;
                $scope.Accounts = User.getUserAccounts(userObj.id);
                $scope.noAccount = false;
                if ($scope.Accounts && $scope.Accounts.length > 0) {
                    var acctObj = CustomerData.getAccount();
                    if (typeof(acctObj) !== "undefined") {
                        $scope.accountNo = acctObj.accountNo;
                        $scope.currency = acctObj.currency;
                        $scope.amount = acctObj.amount;
                    } else {
                        $scope.accountNo = $scope.Accounts[0];
                        accountSelected();
                    }
                } else {
                    $scope.noAccount = true;
                }
                $scope.$on('amountChg', function(event, data) { 
                    var acctObj = CustomerData.getAccount(); 
                    $scope.amount = acctObj.amount;
                });
                $scope.selectAcct = function() {
                    accountSelected();
                    $scope.$broadcast('refresh');               
                }

                $scope.transactions = function(){
                    $state.transitionTo('main.list');
                    $scope.btnClass1 = 'btn-primary';
                    $scope.btnClass2 = '';
                    $scope.btnClass3 = '';
                }
                $scope.deposit = function(){
                    $state.transitionTo('main.account.deposit');
                    $scope.btnClass1 = '';
                    $scope.btnClass2 = 'btn-primary';
                    $scope.btnClass3 = '';
                    
                }
                $scope.withdrawl = function(){
                    $state.transitionTo('main.account.withdrawl');
                    $scope.btnClass1 = '';
                    $scope.btnClass2 = '';
                    $scope.btnClass3 = 'btn-primary';
                    
                }
                function accountSelected () {
                    var acctObj = Account.getAccount($scope.accountNo);
                    $scope.currency = acctObj.currency;
                    $scope.amount = acctObj.amount;
                    CustomerData.setAccount(acctObj);                      
                }
                
            }

accountCtrl.$inject = ['$scope','$state','CustomerData','User','Account','Transaction'];

})();