
### Info

 replica of an oversimplified  streaming XML to JSON translator [repository](https://github.com/AnatoliyDemiurge/XMLparser) in 50 lines of code

### Note
 unfortunatey the original project fails to parse SOAP payloads because complertely unprepared of handling XML attributes 

The `test.xml` of the original project 


```XML
<payment>
    <amount>
      <currency>MXN</currency>
      <quantity>10</quantity>
    </amount>
    <from>Evan</from>
    <to>PayStand</to>
</payment>
```
is too simple compared to SOAP payload - which is attribute and namespace - heavy

Technically one can possibly tweak the `Parser.js` to strip attributes of elements automatically, the attribute-less DOM would still be valid and SOAPAction is the namespace amended tag name. This is a work in progress, the below steps describe the expected value 


### Usage

* Take a basic SOAP payload example:
```XML

<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
  <SOAP-ENV:Body>
    <m:MyMessage xmlns:m="http://electrocommerce.org/abc">
      <m:MyArgument>Hello</m:MyArgument>
    </m:MyMessage>
  </SOAP-ENV:Body>
</SOAP-ENV:Envelope>

```

* temporatily remove every attribute manually
```XML

<SOAP-ENV:Envelope>
  <SOAP-ENV:Body>
    <m:MyMessage>
      <m:MyArgument>Hello</m:MyArgument>
    </m:MyMessage>
  </SOAP-ENV:Body>
</SOAP-ENV:Envelope>
```
* modify the `Parser.js` to take `:` and `-` as a valid tag name characters (may not be in fact necessary) :

```javascript
tag = openTag.match(/[^<][:-\w+$]*/)[0];
```
* run in vanilla node container
```sh 
IMAGE=node:16.12.0-alpine3.11
docker pull $IMAGE
```

```sh
NAME=test
docker run --name $NAME -it $IMAGE sh
```
```sh
for FILE in test.xml app.js Parser.js;do docker cp $FILE $NAME:/ ; done
```
* parse into JSON (in the container)

```sh
node ./app.js
```
```javascript
{
  'SOAP-ENV:Envelope': { 'SOAP-ENV:Body': { 'm:MyMessage': [Object] } }
}

```

* add the line to the demo app `app.js` to illustrate the `Parser.js` loaded a fully defined object:

```javascript
console.log(result['SOAP-ENV:Envelope']['SOAP-ENV:Body']);
```
copy modified script into container and rerun. This time it shows the contents of the `m:MyMessage` element:

```javascript
{
  "SOAP-ENV:Envelope": { "SOAP-ENV:Body": { "m:MyMessage": [Object] } }
}
```
```json
{ "m:MyMessage": { "m:MyArgument": "Hello" } }
```
#### Cleanup
```sh
docker container stop $NAME
docker container rm $NAME
```
### Converting to Elastic  Update script

* basic function
```
POST /apm-7.17.7-transaction-000001/_doc/fnQ1MIUBl_9A8z2FT5_F/_update 
{
    "script": {
          "lang": "painless",
          "source": """
            String parseXML(def source){ return source; }
          String data  = 'data';
          String result = parseXML(data);
          ctx._source.transaction.name = result;
       """,
          "params": {
            "delimiter": "/",
            "position": -1
          }
    }
}
```
* initializing variables, Java - like

```sh
POST /apm-7.17.7-transaction-000001/_doc/fnQ1MIUBl_9A8z2FT5_F/_update 
{
    "script": {
          "lang": "painless",
          "source": """
          String parseXML(def source){ 
            def jsonRes =  new HashMap();
            return source; 
            
          }   
           
            	
          String data  = 'data';
          String result = parseXML(data);
          ctx._source.transaction.name = data;
       """,
          "params": {
            "delimiter": "/",
            "position": -1
          }
    }
}

```
### TODO

  * add exception handler - streamed XML parsing is using recursion and is prone to stack overflows
  * add globbing the contents of the `result['SOAP-ENV:Envelope']['SOAP-ENV:Body']` which is entirely context agnostic, to get some heuristic to get SOAPAction  of a real heavy SOAP XML payloads. Alternatively one can stop right at this level and let ElasticSearch explore the resulting JSON

### See Also 
   * another pure js XML DOM parser [repository](https://github.com/iazrael/xmlparser)
   * another fast and simple streaming XML Parser for parsing xml to json objects [repository](https://github.com/Ahmadreza-s/xmlparser)
   * [painless functions](https://www.elastic.co/guide/en/elasticsearch/painless/current/painless-functions.html)
  * [](https://www.elastic.co/guide/en/elasticsearch/painless/current/painless-walkthrough.html)
  * [advanced scripting engine](https://www.elastic.co/guide/en/elasticsearch/reference/master/modules-scripting-engine.html#modules-scripting-engine) example of a custom ScriptEngine
   * [painless lanuage specification](https://www.elastic.co/guide/en/elasticsearch/painless/current/painless-lang-spec.html) - painless scripting language is rather simple but obscure
   * [how to Script Painless-ly in Elasticsearch](https://www.compose.com/articles/how-to-script-painless-ly-in-elasticsearch/) - does not cover functions

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
