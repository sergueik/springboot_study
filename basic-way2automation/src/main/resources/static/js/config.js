/** Setting the angular module **/

angular.module('BankApp').config(function($stateProvider, $urlRouterProvider) {
    
    $urlRouterProvider.otherwise('/');
    
    $stateProvider

        .state('main', {
            url: '',
            templateUrl: 'main.html',
            abstract:true,
            controller: 'mainCtrl'
        })

        .state('main.options', {
            url: '/login',
            templateUrl: 'options.html',
            controller: 'optionCtrl'
        })

		.state('main.mgrView', {
            url: '/manager',
            templateUrl: 'managerView.html',
            controller: 'managerViewCtrl'
        })

   		.state('main.mgrView.add', {
            url: '/addCust',
            templateUrl: 'newCustomer.html',
            controller: 'addCustomerCtrl'
        })

        .state('main.mgrView.account', {
            url: '/openAccount',
            templateUrl: 'openAccount.html',
            controller: 'openAccountCtrl'
        })

   		.state('main.mgrView.list', {
            url: '/list',
            templateUrl: 'customerList.html',
            controller: 'listCustomerCtrl'
        })

		.state('main.custView', {
            url: '/customer',
            templateUrl: 'customerView.html',
            controller: 'customerViewCtrl'
        })

   		.state('main.account', {
            url: '/account',
            templateUrl: 'account.html',
            controller: 'accountCtrl'
        })

   		.state('main.account.deposit', {
            url: '',
            templateUrl: 'depositTx.html',
            controller: 'depositCtrl'
        })

   		.state('main.account.withdrawl', {
            url: '',
            templateUrl: 'withdrawlTx.html',
            controller: 'withdrawlCtrl'
        })

        .state('main.list', {
            url: '/listTx',
            templateUrl: 'listTx.html',
            controller: 'listTransactionCtrl'
        })
        ;


});
