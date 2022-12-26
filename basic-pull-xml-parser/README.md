### Info

this directory contains a small XML pull parser extracted from 
android xml parser with custom api(pull parser)
[project](https://github.com/rsrahulee/XML)
with added few simple WSDL documents for testing


The repeated pattern to get the `soapAction` attribute is `//wsdl:binding/wsdl:operation/soap:operation@soapAction`

```XML
<wsdl:binding name="EmployeeServiceSOAP" type="tns:EmployeeServicePortType">
    <wsdl:operation name="GetEmployeeById">
      <soap:operation soapAction="http://www.jpworks.com/employee/GetEmployeeById"/>
...
```
it can be specified in a variety formats often longer

```XML
<binding name = "Hello_Binding" type = "tns:Hello_PortType">
      <soap:binding style = "rpc"
         transport = "http://schemas.xmlsoap.org/soap/http"/>
      <operation name = "sayHello">
         <soap:operation soapAction = "sayHello"/>
         <input>
            <soap:body
               encodingStyle = "http://schemas.xmlsoap.org/soap/encoding/"
               namespace = "urn:examples:helloservice"
               use = "encoded"/>
         </input>
...
```

The Actual SOAP messages are a lot simpler and feature application-specific (outside of the SOAP namespace) custom named element tags like below:

* request
```XML

<?xml version="1.0"?>

<soap:Envelope
xmlns:soap="http://www.w3.org/2003/05/soap-envelope/"
soap:encodingStyle="http://www.w3.org/2003/05/soap-encoding">

<soap:Body>
  <m:GetPrice xmlns:m="https://www.w3schools.com/prices">
    <m:Item>Apples</m:Item>
  </m:GetPrice>
</soap:Body>

</soap:Envelope>
```
* response
```XML
<?xml version="1.0"?>

<soap:Envelope
xmlns:soap="http://www.w3.org/2003/05/soap-envelope/"
soap:encodingStyle="http://www.w3.org/2003/05/soap-encoding">

<soap:Body>
  <m:GetPriceResponse xmlns:m="https://www.w3schools.com/prices">
    <m:Price>1.90</m:Price>
  </m:GetPriceResponse>
</soap:Body>

</soap:Envelope>
```

to be able to process that XML a lean version of the parser can be designed since there ill be no comments, DTD, or processing instructions (it is uncertain if the `CDATA` is allowed in the __SOAP__ envelope.

### See Also

  * [simple guide to WSDL](https://www.tutorialworks.com/wsdl/)
  * __XML WSDL__ [documentation] (https://www.w3schools.com/xml/xml_wsdl.asp) illustrating the __WSDL__ Binding to __SOAP__
  * another [WSDL - Example](https://www.tutorialspoint.com/wsdl/wsdl_example.htm)
  * __SOAP__ request and response message [specification with examples](https://www.w3schools.com/xml/xml_soap.asp)
