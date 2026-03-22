import { appRouting } from '/assets/app/js/app-routing.js';
import { loginUserService } from '/assets/app/js/loginUserService.js';
import { bookKeepingService } from '/assets/app/js/bookKeepingService.js';
import { IndexController } from '/assets/app/js/IndexController.js';
import { LoginController } from '/assets/app/js/LoginController.js';

let app = angular.module('sampleApp', ["ngResource", "ui.router"]);
app.config(appRouting);
app.factory("loginUserService", [
   "$resource",
   loginUserService]);
app.factory("bookKeepingService", [
   "$resource",
   bookKeepingService]);
app.controller("IndexController", [
   "$rootScope",
   "$scope",
   "$state",
   "bookKeepingService",
   IndexController
]);
app.controller("LoginController", [
   "$rootScope",
   "$scope",
   "$state",
   "loginUserService",
   LoginController
]);
