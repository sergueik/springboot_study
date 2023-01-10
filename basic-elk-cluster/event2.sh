#!/bin/sh
DATA=$(cat <<EOF)
<?xml version="1.0"?>
<soap:Envelope
xmlns:soap="http://www.w3.org/2003/05/soap-envelope/"
soap:encodingStyle="http://www.w3.org/2003/05/soap-encoding">

<soap:Body>
  <m:UpdatePrice
  xmlns:m="https://www.w3schools.com/prices"
  soap:encodingStyle="http://www.w3.org/2003/05/soap-encoding"
  ><m:Item>
    Apples</m:Item>
  </m:UpdatePrice>
</soap:Body>
EOF
curl -X POST -d "$DATA" http://localhost:6000/call2
