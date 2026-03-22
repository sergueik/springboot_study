export function loginUserService($resource) {
   let retVal = { };
   
   let apiRes = $resource(null, null, {
      authenticateUser: {
         url: "/authenticate",
         method: "post",
         isArray: false
      }
   });
   
   retVal.authenticateUser = function (userName, userPass) {
      return apiRes.authenticateUser({
         userName: userName,
         userPass: userPass
      }).$promise;
   };
   
   return retVal;
}