export function appRouting($stateProvider, $urlRouterProvider) {
   $urlRouterProvider.otherwise("/");
  
   $stateProvider.state("index", {
      url: "/",
      templateUrl: "/assets/app/pages/index.html",
      controller: "IndexController",
      controllerAs: "vm"
   });
   
   $stateProvider.state("login", {
      url: "/login",
      templateUrl : "/assets/app/pages/login.html",
      controller: "LoginController",
      controllerAs: "vm"
   });
   
   $stateProvider.state("notAuthorized", {
      url: "/notAuthorized",
      templateUrl: "/assets/app/pages/notAuthorized.html"
   });
}