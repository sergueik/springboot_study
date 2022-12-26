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

### Grok-Style Element Discovery

To extract the element which has namespace attribute of its own namespace that is different from `soap` namespace, one can utilize regular expression matching each elements in the envelope in turn

```java

String elementMatcher = "<([\\w]+):([\\w]+)\\s*(?:xmlns:)([\\w]+)\\s*=\\s*\"([^\"]+)\">";
Pattern p = Pattern.compile(elementMatcher);
String namespacePrefix = null;
String tagName = null;
String namespaceName = null;
String namespaceUri = null;
Matcher m = p.matcher(element);

if (m.find()) {
	namespacePrefix = m.group(1);
	tagName = m.group(2);
	namespaceName = m.group(3);
	namespaceUri = m.group(4);
```
Appplying this filter will reveal:


| element              |  match            |
|----------------------|--------------|
| `<?xml version="1.0"?>`     | no match.        |
| `<soap:Envelope  xmlns:soap="http://www.w3.org/2003/05/soap-envelope/"  soap:encodingStyle="http://www.w3.org/2003/05/soap-encoding">`     | ignoring soap namepace - no match (needs to rectified by checking namepace prefix)        |
| `<soap:Body>`     | no match.        |
| `<m:GetPrice xmlns:m="https://www.w3schools.com/prices">`     | match: <br/> `namespacePrefix`: `"m"` <br/>   `tagName`: `"GetPrice"` <br/>    `namespaceName`: `"m"`<br/>      `namespaceUri`: `"https://www.w3schools.com/prices"` |
| `<m:Item>Apples`     | no match.        |
| `</m:Item>`     | no match.        |
| `</m:GetPrice>`     | no match.        |
| `</soap:Body>`     | no match.        |
| `</soap:Envelope>`     | no match.        |

the test run log:

```text
input: <?xml version="1.0"?>
no match.
input: <soap:Envelope  xmlns:soap="http://www.w3.org/2003/05/soap-envelope/"  so
ap:encodingStyle="http://www.w3.org/2003/05/soap-encoding">
ignoring soap namepace.
input: <soap:Body>
no match.
input: <m:GetPrice xmlns:m="https://www.w3schools.com/prices">
namespacePrefix: "m"    tagName: "GetPrice"     namespaceName: "m"      namespac
eUri: "https://www.w3schools.com/prices"
input: <m:Item>Apples
no match.
input: </m:Item>
no match.
input: </m:GetPrice>
no match.
input: </soap:Body>
no match.
input: </soap:Envelope>
no match.
```
### See Also

  * [simple guide to WSDL](https://www.tutorialworks.com/wsdl/)
  * __XML WSDL__ [documentation] (https://www.w3schools.com/xml/xml_wsdl.asp) illustrating the __WSDL__ Binding to __SOAP__
  * another [WSDL - Example](https://www.tutorialspoint.com/wsdl/wsdl_example.htm)
  * __SOAP__ request and response message [specification with examples](https://www.w3schools.com/xml/xml_soap.asp)
