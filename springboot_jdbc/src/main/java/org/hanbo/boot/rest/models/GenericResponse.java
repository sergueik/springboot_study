package org.hanbo.boot.rest.models;

public class GenericResponse
{
   private boolean success;
   
   private String statusMsg;

   public boolean isSuccess()
   {
      return success;
   }

   public void setSuccess(boolean success)
   {
      this.success = success;
   }

   public String getStatusMsg()
   {
      return statusMsg;
   }

   public void setStatusMsg(String statusMsg)
   {
      this.statusMsg = statusMsg;
   }
}
