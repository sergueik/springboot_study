       01  TELLER-TRANSACTION-REC.
           05  TT-HEADER.
               10  TT-BRANCH-ID          PIC X(05).
               10  TT-TELLER-ID          PIC X(06).
               10  TT-TERMINAL-ID        PIC X(04).
               10  TT-TRAN-DATE          PIC 9(08).  *> YYYYMMDD
               10  TT-TRAN-TIME          PIC 9(06).  *> HHMMSS

           05  TT-BODY.
               10  TT-ACCOUNT-NUMBER     PIC 9(12).
               10  TT-TRAN-CODE          PIC X(04).
               10  TT-AMOUNT             PIC S9(11)V99 COMP-3.
               10  TT-CURRENCY           PIC X(03).
               10  TT-DESCRIPTION        PIC X(30).

           05  TT-STATUS.
               10  TT-RESPONSE-CODE      PIC X(02).
               10  TT-APPROVAL-CODE      PIC X(06).

