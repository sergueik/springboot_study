package org.hanbo.boot.app.models;

public class LoginRequest
{
   private String userName;
   
   private String userPass;

   public String getUserName()
   {
      return userName;
   }

   public void setUserName(String userName)
   {
      this.userName = userName;
   }

   public String getUserPass()
   {
      return userPass;
   }

   public void setUserPass(String userPass)
   {
      this.userPass = userPass;
   }
}
