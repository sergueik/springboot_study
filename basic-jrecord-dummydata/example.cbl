       01 SAMPLE-REC.
      * comment
         03 ACCOUNT-PLAIN.
           05 CUSTOMER-ID            PIC X(10).
           05 FILLER REDEFINES CUSTOMER-ID.
             07 BYTE1 PIC X.
             07 BYTE2 PIC X.
           05 CUSTOMER-NAME          PIC X(20).
           05 ACCOUNT-NUMBER         PIC 9(9).
           05 ACCOUNT-NUMBER-ALPHA REDEFINES ACCOUNT-NUMBER PIC X(9).
           05 ACCOUNT-TYPE           PIC X(2).
           05 OPEN-DATE              PIC 9(8).
           05 BALANCE                PIC S9(7)V99 COMP-3.
           05 CREDIT-LIMIT           PIC S9(7)V99 COMP-3.
           05 STATUS-CODE            PIC X(1).
           05 LAST-ACTIVITY-DATE     PIC 9(8).
           05 RESERVED-FLAG          PIC X(1).
        03 ACCOUNT-LINK REDEFINES ACCOUNT-PLAIN.
           05 ACC-ONLINE-KEY.
             07 ACC-CONTROL.
               09 FIELD1 PIC XX.
               09 FIELD2 PIC XXX.
               09 FIELD3 PIC X(10).
           05 SOMETHING REDEFINES ACC-ONLINE-KEY.
             07 NUM_DAYS PIC S999 COMP-3.
      * alternate layout
        03 ACCOUNT-FIXED REDEFINES ACCOUNT-PLAIN.
           05 ACC3-FIXED1 VALUE '01'.
           05 ACC3-FIXED2 VALUE 'C'.
           05 ACC3-FIXED3 VALUE 'E'.
           05 FILLER PIC X.

