      *                                                                         
      *   DTAR020 IS THE OUTPUT FROM DTAB020 FROM THE IML                       
      *   CENTRAL REPORTING SYSTEM                                              
      *                                                                         
      *   CREATED BY BRUCE ARTHUR  19/12/90                                     
      *                                                                         
      *   RECORD LENGTH IS 27.                                                  
      *       
         01  sss.
             03  DTAR020-KCODE-STORE-KEY.                                      
                 05 DTAR020-KEYCODE-NO      Pic X(08).                         
                 05 DTAR020-STORE-NO        PIC S9(03)   COMP-3.               
             03  DTAR020-DATE               PIC S9(07)   COMP-3.               
             03  DTAR020-DEPT-NO            PIC S9(03)   COMP-3.               
             03  DTAR020-QTY-SOLD           PIC S9(9)    COMP-3.               
             03  DTAR020-SALE-PRICE         PIC S9(9)V99 COMP-3.               

