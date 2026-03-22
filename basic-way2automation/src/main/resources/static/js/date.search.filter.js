(function() {
  'use strict';
  angular.module('BankApp').filter('sDate',sDate);
  
  function sDate() {
    return function(items,start,end) {
      var filtered = [];
      angular.forEach(items, function(item) {
        if(item.date >= start && item.date <= end) {
          filtered.push(item);
        }
      });
      return filtered;
    };
  }

})();