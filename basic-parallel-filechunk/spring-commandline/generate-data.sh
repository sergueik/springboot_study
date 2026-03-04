#!/bin/sh 
mkdir -p copybooks/good copybooks/bad

cat > copybooks/good/simple-fixed.cbl <<'EOF'
01  SAMPLERECORD.
05  RECTYPE           PIC X(1).
05  ACCOUNTNUMBER     PIC 9(5).
05  CUSTOMERNAME      PIC X(20).
EOF

cat > copybooks/good/comp3-fields.cbl <<'EOF'
01  FINANCIAL-REC.
05  ACCOUNT-ID        PIC 9(10).
05  BALANCE           PIC S9(7)V99 COMP-3.
05  TX-COUNT          PIC 9(3) COMP-3.
EOF

cat > copybooks/good/occurs-array.cbl <<'EOF'
01  ARRAY-REC.
05  ITEM-COUNT        PIC 9(2).
05  ITEMS OCCURS 5 TIMES.
10  ITEM-ID           PIC 9(4).
10  ITEM-NAME         PIC X(10).
EOF

cat > copybooks/good/with-88-levels.cbl <<'EOF'
01  STATUS-REC.
05  STATUS-CODE       PIC X.
88  STATUS-OK         VALUE 'Y'.
88  STATUS-FAIL       VALUE 'N'.
EOF

cat > copybooks/bad/example-dc3aa31c.cbl <<'EOF'
01  SAMPLERECORD.
05  RECTYPE           PIC X(1).
05  ACCOUNTNUMBER     PIC 9(5).
05  CUSTOMERNAME      PIC X(20).
05  BALANCEAMOUNT     PIC S9(7)V99 COMP-3.
05  TRANSACTIONCOUNT  PIC 9(3) COMP-3.
05  STATUSCODE        PIC 9(1).
EOF

cat > copybooks/bad/missing-level.cbl <<'EOF'
SAMPLERECORD.
05  FIELD1 PIC X(10).
EOF

cat > copybooks/bad/garbage-token.cbl <<'EOF'
01  TEST-REC.
05  FIELD1 PIC X(10).
THIS IS NOT COBOL.
EOF

cat > copybooks/bad/bad-88-placement.cbl <<'EOF'
01  BROKEN-REC.
05  FIELD1 PIC X.
05  FIELD2 PIC 9.
88  INVALID-88 VALUE 1.
EOF

"/c/Program Files/7-Zip/7z.exe" a copybooks.zip copybooks 
base64 copybooks.zip > copybooks.zip.b64
