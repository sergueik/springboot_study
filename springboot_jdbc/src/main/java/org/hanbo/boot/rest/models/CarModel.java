package org.hanbo.boot.rest.models;

public class CarModel
{
   private int yearOfManufacturing;
   
   private String model;
   
   private String maker;
   
   private float suggestedRetailPrice;
   
   private float fullPrice;
   
   private float rebateAmount;

   public int getYearOfManufacturing()
   {
      return yearOfManufacturing;
   }

   public void setYearOfManufacturing(int yearOfManufacturing)
   {
      this.yearOfManufacturing = yearOfManufacturing;
   }

   public String getModel()
   {
      return model;
   }

   public void setModel(String model)
   {
      this.model = model;
   }

   public String getMaker()
   {
      return maker;
   }

   public void setMaker(String maker)
   {
      this.maker = maker;
   }

   public float getSuggestedRetailPrice()
   {
      return suggestedRetailPrice;
   }

   public void setSuggestedRetailPrice(float suggestedRetailPrice)
   {
      this.suggestedRetailPrice = suggestedRetailPrice;
   }

   public float getFullPrice()
   {
      return fullPrice;
   }

   public void setFullPrice(float fullPrice)
   {
      this.fullPrice = fullPrice;
   }

   public float getRebateAmount()
   {
      return rebateAmount;
   }

   public void setRebateAmount(float rebateAmount)
   {
      this.rebateAmount = rebateAmount;
   }
}
