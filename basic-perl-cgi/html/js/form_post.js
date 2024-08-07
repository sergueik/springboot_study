var app = angular.module('postserviceApp', []);
// one does have to add the $location controller handler argument
app.controller('postserviceCtrl', function($scope, $location, $http) {
  $scope.name = null;
  $scope.age = null;
  $scope.adress = null;
  $scope.lblMsg = null;
  $scope.postdata = function(name, age, adress) {
    var data = {
      name: name,
      age: age,
      adress: adress
    };
    // https://docs.angularjs.org/api/ng/service/$http#$http-arguments
    // NOTE: cannot exercise CORS with host names in minimal cluster connected to development host via port mapping, would need to add a container to have the X Desktop and GUI to run the client browser
    // alternatively one can use vanilla javascript window.location.hostname or simply location.hostname
    // - not the window.location.host which includes the port
    // https://docs.angularjs.org/api/ng/service/$location#host
    // use a different port to trigger the CORS checks
    const port = 9090;
    // console.log('http://' + $location.host() + ':9090/cgi-bin/echo_json.cgi') ;
    // NOTE: the AngularJS $location.url() is getter / setter method, but it appear to return empty string - its main purpose apperently is different: for building the browser location 
    // https://stackoverflow.com/questions/34017904/window-location-href-not-working-in-angularjs
    // https://docs.angularjs.org/api/ng/service/$location#url
    var url = $location.url();
    // console.log('location.url: ' + url);
    if (url === null || url == '') {
      url = document.URL;
    }
    data['custom_referrer'] = url;
    $http.post('http://' + $location.host() + ':' + port + '/cgi-bin/echo_json.cgi', JSON.stringify(data), {
      headers: {
        // Referer: $location.url()
        // angular.js:11945 Refused to set unsafe header "Referer"
        'Referrer-Policy': 'origin'
      }
    }).then(function(response /* data, status, headers, config */
      // NOTE: Angular refused to set unsafe header "Referer"
      // https://stackoverflow.com/questions/33143776/ajax-request-refused-to-set-unsafe-header
      //  https://en.wikipedia.org/wiki/List_of_HTTP_header_fields
      // https://en.wikipedia.org/wiki/HTTP_referer
      // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Referrer-Policy
      // NOTE: setting the Referrer-Policy header has no effect
      // https://docs.angularjs.org/api/ng/service/$http#$http-returns
    ) {
      if (response.data)
        $scope.msg = "Post Data Submitted Successfully! Response: " + JSON.stringify(response.data);
      console.log(response.data);
    }, function(response) {
      $scope.msg = "Service not Exists";
      $scope.statusval = response.status;
      $scope.statustext = response.statusText;
      $scope.headers = response.headers();
    });
  };
});
