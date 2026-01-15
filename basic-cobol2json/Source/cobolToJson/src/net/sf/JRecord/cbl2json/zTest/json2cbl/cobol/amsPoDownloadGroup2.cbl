      *******************************************************************
      *  Ams PO download file
      *******************************************************************
               
        01 PO-Record.   
           05 Record-Type            Pic X(2).
           05 Sequence-Number        Pic 99v999. 
           05 Vendor-PO-Details.
              10 Vendor              Pic 9(10). 
              10 PO-Details.
                 15 PO               Pic 9(12).  
                 15 Entry-Date.
                    20 year          pic 99.
                    20 month         pic 99.
                    20 day-of-month  pic 99.
           05 Filler                 Pic X(8).
           05 codes.
              10 beg01-code          Pic X(2).
              10 beg02-code          Pic X(2).
           05 Department             Pic X(4).
           05 The-Dates.
              10 Expected-Reciept-Date.
                 20 year             pic 99.
                 20 month            pic 99.
                 20 day-of-month     pic 99.
              10 Cancel-by-date.
                 20 year             pic 99.
                 20 month            pic 99.
                 20 day-of-month     pic 99.
           05 Filler                 Pic X(4).
           05 EDI-Type               Pic X(1).
           05 Add-Date.
              20 year                pic 99.
              20 month               pic 99.
              20 day-of-month        pic 99.
           05 Filler                 Pic X(1).
           05 Department-Name        Pic X(10).
           05 Prcoess-Type           Pic X(1).
           05 Order-Type             Pic X(2).
      
        01 Product-Record.
           05 Record-Type            pic xx.
           05 Pack-details.
              10 Pack-Qty            Pic 9(5)V9999.
              10 Pack-Cost           Pic 9(9)V9999.
           05 Product-details.
              10 APN                 Pic 9(13).                                           .
              10 Filler              Pic X(1).
           05 Product                Pic 9(8).
           05 Filler                 Pic X(25).
           05 pmg-dtl-tech-key       Pic X(15).
           05 Case-Pack-id           Pic X(15).
           05 Product-Name           Pic X(50).
         
        01 Location-Record.
           05 Record-Type            pic xx.
           05 location occurs 10.
              10 DC-Number           pic 9(4).
              10 Pack-Quantity       pic 9(8).         

