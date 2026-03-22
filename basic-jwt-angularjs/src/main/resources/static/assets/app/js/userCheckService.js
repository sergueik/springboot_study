export function setSessionCurrentUser(userToAdd) {
   if (userToAdd != null &&
         userToAdd.userId &&
         userToAdd.userId.trim() !== "" &&
         userToAdd.authToken &&
         userToAdd.authToken.trim() !== "") {
      if (sessionStorage.currentUser) {
         sessionStorage.currentUser = null;
      }
      
      sessionStorage.currentUser = JSON.stringify(userToAdd);  
   }
}

export function removeSessionCurrentUser() {
   sessionStorage.currentUser = null;
   sessionStorage.removeItem("currentUser");
}

export function checkUserLoggedIn() {
   // XXX refactor needed
   let retVal = false;
   if (sessionStorage.currentUser &&
         sessionStorage.currentUser.length > 0) {
      let currUser = JSON.parse(sessionStorage.currentUser);
      if (currUser &&
            currUser.userId &&
            currUser.userId.trim() !== "") {
         retVal = currUser.authToken && currUser.authToken.trim() !== "";
      }
   }
     
   return retVal;
}

export function getLoggedinUser() {
   let retVal = null;
   if (sessionStorage.currentUser &&
         sessionStorage.currentUser.length > 0) {
      let currUser = JSON.parse(sessionStorage.currentUser);
      if (currUser &&
            currUser.userId &&
            currUser.userId.trim() !== "") {
         retVal = currUser;
      }
   }
     
   return retVal;
}

export function getUserSecurityToken() {
   // XXX refactor needed
   let retVal = "";
   if (sessionStorage.currentUser &&
         sessionStorage.currentUser.length > 0) {
      let currUser = JSON.parse(sessionStorage.currentUser);
      if (currUser &&
            currUser.userId &&
            currUser.userId.trim() !== "") {
         if (currUser.authToken && currUser.authToken.trim() !== "") {
            retVal = currUser.authToken;
         }
      }
   }
     
   return retVal;
}