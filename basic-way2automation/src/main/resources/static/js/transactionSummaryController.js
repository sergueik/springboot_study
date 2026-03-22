(function() {
  "use strict";
  angular.module('BankApp').controller('listTransactionCtrl',listTransactionCtrl);

    function listTransactionCtrl($scope,$timeout,$state,$filter,$anchorScroll, $location,Transaction,Account,CustomerData){
                $scope.$parent.logout = true;
                $scope.sortReverse = false;
                $scope.left = false;
                $scope.right = true;
                var x = 0;
                
                $scope.$on('refresh',function(event,data) {
                    $scope.transactions = Transaction.getTransactions(CustomerData.getUser().id,CustomerData.getAccount().accountNo);
                });
                var txs = Transaction.getTransactions(CustomerData.getUser().id,CustomerData.getAccount().accountNo);

                if (typeof(txs) !== "undefined" && txs !== null && txs.length > 0 ){
                    $scope.transactions = $filter('orderBy')(txs,'date',false);
                    $scope.startDate = $scope.transactions[0].date;
                    $scope.end = $scope.transactions[$scope.transactions.length-1].date;
                    $scope.showDate = true;  
                    // if ($scope.startDate !== $scope.end) {
                        $scope.minSDate = $scope.transactions[0].date;
                        $scope.maxSDate = $scope.end;
                        $scope.minEDate = $scope.startDate;
                        $scope.maxEDate = new Date();                        
                    // }
                    if ($scope.startDate === $scope.end) {
                      $scope.maxSDate = new Date();
                    }
                } else {
                    
                    $scope.transactions = [];
                    $scope.showDate = false;
                }

                $scope.reset = function() {
                    Transaction.deleteTx(CustomerData.getUser().id,CustomerData.getAccount().accountNo);
                    $scope.amount = 0;
                    $scope.$broadcast('refresh');
                    $timeout(function() {
                        Account.saveObj();
                        Transaction.saveObj();
                    }, 0);
                }

                $scope.scrollLeft = function() {
                    x = x - 8;
                    var id = 'anchor';
                    if (x > 0) {
                        id = id + x; 
                    } 
                    $timeout(function() {
                        $location.hash(id);
                        $anchorScroll();
                    });                        
                    if (x <= 0){
                        $scope.left = false;
                    }

                }
                $scope.scrollRight = function() {
                    $scope.left = true;
                    if (x===0) x = 5;
                    else x = x + 8;
                    if (typeof($('#anchor'+x).val()) === "undefined") {
                        if (x==5) {
                            x=0;
                            $scope.left = false;
                        } else x = x - 8;
                    } else {
                        $timeout(function() {
                            $location.hash("anchor"+x);
                            $anchorScroll();
                        });                        
                    }
                }
                $scope.scrollTop = function() {
                    x=0;
                    $timeout(function() {
                        $location.hash("anchor");
                        $anchorScroll();
                    });   
                }
                $scope.back = function() {
                    $state.transitionTo('main.account');
                }
            }
listTransactionCtrl.$inject = ['$scope','$timeout','$state','$filter','$anchorScroll', '$location','Transaction','Account','CustomerData'];

})();