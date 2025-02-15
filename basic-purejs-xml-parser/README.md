
### Info

 replica of an oversimplified streaming XML to JSON parser [repository](https://github.com/ErickWeeds/XMLParser) 
 capable of converting attribute-less DOM to JSON in 50 lines of code

### Note
 the original project fails with SOAP payloads because completely unprepared of handling XML attributes

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
for a faily large XML file without attributes usage consider ageneric Java project `pom.xml`. 
A sample file is checked in  locally. 
The only modification needed to make it parseable was replace empty   element
```XML
<relativePath/>
```
with an explicit pair of open/ close tags:
```XML
<relativePath></relativePath>
```
 and remove the XML comments with XML elements commented:
```XML
<!--
<element> ... 
-->
```

Parsing such file will prove the tool is capable of real work:
```sh
node app.js
```
```text
tag: "project"
tag: "modelVersion"
tag: "groupId"
tag: "artifactId"
tag: "version"
tag: "packaging"
tag: "name"
tag: "description"
tag: "parent"
tag: "groupId"
tag: "artifactId"
tag: "version"
tag: "relativePath"
tag: "properties"
tag: "project.build.sourceEncoding"
tag: "project.reporting.outputEncoding"
tag: "java.version"
tag: "finalName"
tag: "gson.version"
tag: "dependencies"
tag: "dependency"
tag: "groupId"
tag: "artifactId"
tag: "version"
tag: "exclusions"
tag: "exclusion"
tag: "groupId"
tag: "artifactId"
tag: "dependency"
tag: "groupId"
tag: "artifactId"
converting value to array: "dependency"
tag: "dependency"
tag: "groupId"
tag: "artifactId"
converting value to array: "dependency"
tag: "build"
tag: "plugins"
tag: "plugin"
tag: "groupId"
tag: "artifactId"
tag: "configuration"
tag: "finalName"
```
```Javascript
Map(1) {
  'project' => Map(11) {
    'modelVersion' => '4.0.0',
    'groupId' => 'example',
    'artifactId' => 'basic',
    'version' => '0.5.0-SNAPSHOT',
    'packaging' => 'jar',
    'name' => 'basic',
    'description' => 'DemoSpringbootDocker-hostedapp',
    'parent' => Map(4) {
      'groupId' => 'org.springframework.boot',
      'artifactId' => 'spring-boot-starter-parent',
      'version' => '1.5.4.RELEASE',
      'relativePath' => ''
    },
    'properties' => Map(5) {
      'project.build.sourceEncoding' => 'UTF-8',
      'project.reporting.outputEncoding' => 'UTF-8',
      'java.version' => '1.8',
      'finalName' => '${project.groupId}.${project.artifactId}',
      'gson.version' => '2.8.9'
    },
    'dependencies' => Map(1) { 'dependency' => [Array] },
    'build' => Map(1) { 'plugins' => [Map] }
  }
}

Technically one can possibly tweak the `Parser.js` to strip attributes of elements automatically, the attribute-less DOM would still be valid and SOAPAction is the namespace amended tag name.


### Usage

* Take a basic SOAP payload example:
```XML

<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
  <SOAP-ENV:Body>
    <m:MyMessage xmlns:m="http://electrocommerce.org/abc">
      <m:MyArguments>Hello</m:MyArguments>
      <m:MyArguments>World</m:MyArguments>
    </m:MyMessage>
  </SOAP-ENV:Body>
</SOAP-ENV:Envelope>

```

* temporatily remove every attribute manually
```XML
<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
  <SOAP-ENV:Body>
    <m:MyMessage xmlns:m="http://electrocommerce.org/abc">
      <m:MyArguments>Hello</m:MyArguments>
      <m:MyArguments>World</m:MyArguments>
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
Map(1) {
  'SOAP-ENV:Envelope' => Map(1) { 'SOAP-ENV:Body' => Map(1) { 'm:MyMessage' => [Map] } }
}
```

* add the line to the demo app `app.js` to illustrate the `Parser.js` loaded a fully defined object:

```javascript
console.log(result['SOAP-ENV:Envelope']['SOAP-ENV:Body']);
```
copy modified script into container and rerun. This time it shows the contents of the `m:MyMessage` element:

```javascript
Map(1) {
  'SOAP-ENV:Envelope' => Map(1) { 'SOAP-ENV:Body' => Map(1) { 'm:MyMessage' => [Map] } }
}
```
```json
Map(1) {
  'm:MyMessage' => Map(1) { 'm:MyArguments' => [ 'Hello', 'World' ] }
}

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
* regex [example](https://www.elastic.co/guide/en/elasticsearch/painless/master/painless-walkthrough.html)


```sh
POST /apm-7.17.7-transaction-000001/_doc/fnQ1MIUBl_9A8z2FT5_F/_update
{
    "script": {
          "lang": "painless",
          "source": """
          String data  = 'data base values';
          Pattern p = /\n|\t|\r| /;
          def result =  p.matcher(data).replaceAll('');


          ctx._source.transaction.name = data;
       """
    }
}


```
NOTE: cannot   enter sample data with newline:
```javascript
String data  = 'data base ' + '\\n' + 'values';
```

wraping into function

```sh

POST /apm-7.17.7-transaction-000001/_doc/fnQ1MIUBl_9A8z2FT5_F/_update
{
    "script": {
          "lang": "painless",
          "source": """
            String unescapeString(def data){

            Pattern p = /\n|\t|\r| /;
            def result =  p.matcher(data).replaceAll('');
            return result;
            }
          String data  = 'data base values x="y"';
          ctx._source.transaction.name = unescapeString(data);
       """
    }
}

```
* combining two functions:

```sh
POST /apm-7.17.7-transaction-000001/_doc/fnQ1MIUBl_9A8z2FT5_F/_update
{
    "script": {
          "lang": "painless",
          "source": """
            String unescapeString(def data){

            Pattern p1 = /\n|\t|\r/;
            def result1 =  p1.matcher(data).replaceAll(' ');
            def result2 = clearAttributes(result1);
            Pattern p2 = / /;
            def result3 =  p2.matcher(result2).replaceAll('');
            return result3;
            }
            String clearAttributes(def data){

            Pattern p = /[\w:-]+ *= *"[^"]+"/;
            def result =  p.matcher(data).replaceAll('');
            return result;
            }
          String data  = 'data base values x="y"';
          ctx._source.transaction.name = unescapeString(data);
       """
    }
}
```

This will lead to an exception:
```
circuit_breaking_exception
reason:
[scripting] Regular expression considered too many characters
pattern: [[\\w:-]+ *= *\"[^\"]+\"]
limit factor: [6], char limit: [132], count: [133],
wrapped: [data base values x=\"y\"],
this limit can be changed by changed
by the [script.painless.regex.limit-factor] setting
```
* split the removal of attribute into 2 steps:

```sh
POST /apm-7.17.7-transaction-000001/_doc/fnQ1MIUBl_9A8z2FT5_F/_update
{
    "script": {
          "lang": "painless",
          "source": """
            String unescapeString(def data){

            Pattern p1 = /\n|\t|\r/;
            def result1 =  p1.matcher(data).replaceAll(' ');
            def result2 = clearAttributesPart1(result1);
            Pattern p2 = / /;
            def result3 =  p2.matcher(result2).replaceAll('');
            return result3;
            }
            String clearAttributesPart1(def data){

            Pattern p = /"[^"]+"/;
            def result =  p.matcher(data).replaceAll('ZZZ');
            return result;
            }

            String clearAttributesPart2(def data){

            Pattern p = /[\w:]+ *= *ZZZ"/;
            def result =  p.matcher(data).replaceAll('');
            return result;
            }
          String data  = 'data base values x="y"';
          ctx._source.transaction.name = unescapeString(data);
       """
    }
}

```
does not help:
```sh

POST /apm-7.17.7-transaction-000001/_doc/fnQ1MIUBl_9A8z2FT5_F/_update
{
    "script": {
          "lang": "painless",
          "source": """
            String unescapeString(def data){

            Pattern p1 = /\n|\t|\r/;
            def result1 =  p1.matcher(data).replaceAll(' ');
            def result2 = clearAttributes(result1);
            Pattern p2 = / /;
            def result3 =  p2.matcher(result2).replaceAll('');
            return result3;
            }

            String clearAttributes(def data){
               def result1 = clearAttributesPart1(data);
               def result2 = clearAttributesPart2(result1);
               return result2;
            }

            String clearAttributesPart1(def data){

            Pattern p = /"[^"]+"/;
            def result =  p.matcher(data).replaceAll('ZZZ');
            return result;
            }

            String clearAttributesPart2(def data){

            Pattern p = /[a-z0-9:-]+ *= *ZZZ"/;
            def result =  p.matcher(data).replaceAll('');
            return result;
            }
          String data  = 'data base values x="y"';
          ctx._source.transaction.name = unescapeString(data);
       """
    }
}
```
NOTE:
making the input longer to suppress circuit breaker exception

```javascript
String data  = 'data base values x="y" other="12345" attribute4="foo"';
```
does not appear to work:  the exception becomes
```json
 {
        "type" : "circuit_breaking_exception",
        "reason" : "[scripting] Regular expression considered too many characters, pattern: [[a-z0-9:-]+ *= *ZZZ\"], limit factor: [6], char limit: [282], count: [283], wrapped: [data base values x=ZZZ other=ZZZ attribute4=ZZZ], this limit can be changed by changed by the [script.painless.regex.limit-factor] setting",
        "bytes_wanted" : 0,
        "bytes_limit" : 0,
        "durability" : "TRANSIENT"
      }
```
is there a rounding error?


* update elasticsearch configuration:

```text
script.painless.regex.enabled: true
```
rebuilding the container

makes operation execute successfully:

```sh
GET /apm-7.17.7-transaction-000001/_search?_source=_id
```
```json
{
  "took" : 10,
  "timed_out" : false,
  "_shards" : {
    "total" : 1,
    "successful" : 1,
    "skipped" : 0,
    "failed" : 0
  },
  "hits" : {
    "total" : {
      "value" : 2,
      "relation" : "eq"
    },
    "max_score" : 1.0,
    "hits" : [
      {
        "_index" : "apm-7.17.7-transaction-000001",
        "_type" : "_doc",
        "_id" : "u9WKMYUB7ikC1HY8-Jan",
        "_score" : 1.0,
        "_source" : { }
      },
      {
        "_index" : "apm-7.17.7-transaction-000001",
        "_type" : "_doc",
        "_id" : "t9WKMYUB7ikC1HY84JYH",
        "_score" : 1.0,
        "_source" : { }
      }
    ]
  }
}

```
```sh
GET /apm-7.17.7-transaction-000001/_doc/t9WKMYUB7ikC1HY84JYH?_source=transaction
```

```json
{
  "_index" : "apm-7.17.7-transaction-000001",
  "_type" : "_doc",
  "_id" : "t9WKMYUB7ikC1HY84JYH",
  "_version" : 3,
  "_seq_no" : 3,
  "_primary_term" : 1,
  "found" : true,
  "_source" : {
    "transaction" : {
      "result" : "HTTP 2xx",
      "duration" : {
        "us" : 99370
      },
      "name" : "databasevalues",
      "span_count" : {
        "dropped" : 0,
        "started" : 1
      },
      "id" : "a35487a97c627d6c",
      "type" : "request",
      "sampled" : true
    }
  }
```
also can use the original one step attribute removal:
```sh
POST /apm-7.17.7-transaction-000001/_doc/t9WKMYUB7ikC1HY84JYH/_update
{
    "script": {
          "lang": "painless",
          "source": """
            String unescapeString(def data){

            Pattern p1 = /\n|\t|\r/;
            def result1 =  p1.matcher(data).replaceAll(' ');
            def result2 = clearAttributes(result1);
            Pattern p2 = / /;
            def result3 =  p2.matcher(result2).replaceAll('');
            return result3;
            }
          String clearAttributes(def data){

            Pattern p = /[\w:-]+ *= *"[^"]+"/;
            def result =  p.matcher(data).replaceAll('');
            return result;
            }
          String data  = 'data base values x="y"';
          ctx._source.transaction.name = unescapeString(data);
       """
    }
}

```
NOTE: the `update-via-query` does not appear to be reliable.

* rewrite to compact:
```sh
POST /apm-7.17.7-transaction-000001/_doc/t9WKMYUB7ikC1HY84JYH/_update
{
    "script": {
          "lang": "painless",
          "source": """
            String unescapeString(def data){
              return / /.matcher(clearAttributes(/\n|\t|\r/.matcher(data).replaceAll(' '))).replaceAll('');
            }
            String clearAttributes(def data){
              return /[\w:-]+ *= *"[^"]+"/.matcher(data).replaceAll('');
            }
            String data  = '<SOAP-ENV:Envelope  xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"  SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">';
            ctx._source.transaction.name = unescapeString(data);
       """
    }

}
```
```sh	
GET /apm-7.17.7-transaction-000001/_doc/t9WKMYUB7ikC1HY84JYH?_source=transaction

```
```json
{
  "_index" : "apm-7.17.7-transaction-000001",
  "_type" : "_doc",
  "_id" : "t9WKMYUB7ikC1HY84JYH",
  "_version" : 13,
  "_seq_no" : 18,
  "_primary_term" : 1,
  "found" : true,
  "_source" : {
    "transaction" : {
      "result" : "HTTP 2xx",
      "duration" : {
        "us" : 99370
      },
      "name" : "<SOAP-ENV:Envelope>",
      "span_count" : {
        "dropped" : 0,
        "started" : 1
      },
      "id" : "a35487a97c627d6c",
      "type" : "request",
      "sampled" : true
    }
  }
}


```
NOTE:

some basic functions are unavailable?
```javascript
             // def startPos = openTag.length - 1;
```

leads to error
```text

dynamic getter [java.lang.String, length] not found
```
the important part of XML processor is loader of the element content:
convert to painless

```sh
POST /apm-7.17.7-transaction-000001/_doc/t9WKMYUB7ikC1HY84JYH/_update
{
    "script": {
          "lang": "painless",
          "source": """
            String unescapeString(def data){
              return / /.matcher(clearAttributes(/\n|\t|\r/.matcher(data).replaceAll(' '))).replaceAll('');
            }
            String clearAttributes(def data){
              return /[\w:-]+ *= *"[^"]+"/.matcher(data).replaceAll('');

            }
            String getElementContent(def data, def openTag){
              def closeTag = /</.matcher(openTag).replaceFirst('</');
              def startPos = openTag.indexOf('>') + 1;
              def endPos = data.indexOf(closeTag);
              return data.substring(startPos, endPos);
            }
            String data  = '<SOAP-ENV:Envelope  xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"  SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">some data</SOAP-ENV:Envelope>';
            String openTag = '<SOAP-ENV:Envelope>';
            ctx._source.transaction.name = getElementContent(unescapeString(data), openTag);
       """
    }

}
```

### Painless Script
* after 99% conversion of the `Parser.js` to Painless: 
```Java
{
    "script": {
        "lang": "painless",
        "source": """
          HashMap parseXML(def source) {
            def result = new HashMap();
             def data = unescapeString(source);
              while (data =~/<[^\/][^>]*>/){
                String openTag = findTag(data);
                String tag = openTag.substring(1, openTag.length() - 1);
                def value = getElementContent(data, openTag);
                def tmp = '';
                if (value =~/ <[^\/][^>]*>/) {
                  // found XML inside the element
                  tmp = parseXML(value);
                } else {
                  // found plain text inside the element
                  tmp = value;
                }
                // TODO: handle arrays
                result[tag] = tmp;
            }  
            return result;
          }

            String findTag(def source) {
              String openTag = '';               
              def m = /(<[^\/][^>]*>)/.matcher(source);
              if ( m.find() ) {
                openTag = m.group(1);
              }
              return openTag;
            }
            String unescapeString(def data){
              return / /.matcher(clearAttributes(/\n|\t|\r/.matcher(data).replaceAll(' '))).replaceAll('');
            }
            String clearAttributes(def data){
              return /[\w:-]+ *= *"[^"]+"/.matcher(data).replaceAll('');

            }
            String getElementContent(def data, def openTag){
              String closeTag = /</.matcher(openTag).replaceFirst('</');
              int startPos = openTag.length();
              // NOTE: dynamic getter [java.lang.String, length] not found
              // when property confused with method
              // def startPos = openTag.length - 1;
              // int startPos = openTag.indexOf('>') + 1;
              int endPos = data.indexOf(closeTag);
              if (endPos == -1 ) {
                endPos = data.length() - 1;
              }
              return data.substring(startPos, endPos);
            }

        String data = params['data'];
        def result  = parseXML(data);
        def result2 = result.keySet().asList().toString();
        // [<payment>, <currency>, <amount>]
        ctx._source.transaction.name = result2; 
        """,
        "params": {
            "data": """
              <payment>
                <amount>
                  <currency>MXN</currency>
                  <quantity>10</quantity>
                </amount>
                <from>Evan</from>
                <to>PayStand</to>
            </payment>
            """,
            "position": -1
        }
    }
}
```
the recursion call in processing the element contents 

```java

  HashMap parseXML(def source) {
    def result = new HashMap();
     def data = unescapeString(source);
      while (data =~/<[^\/][^>]*>/){
        String openTag = findTag(data);
        String tag = openTag.substring(1, openTag.length() - 1);
        def value = getElementContent(data, openTag);
        def tmp = '';
        if (value =~/ <[^\/][^>]*>/) {
          // found XML inside the element
          tmp = parseXML(value);
        } else {
          // found plain text inside the element
          tmp = value;
        }
        // TODO: handle arrays
        result[tag] = tmp;
    }  
    return result;
  }

```
which is the key for loading the XML into the tree structure was observed to lead to a runtime error clearly related to the [circuit breaker setting](https://www.elastic.co/guide/en/elasticsearch/reference/current/circuit-breaker.html) enforced (quite reasonably) in the Elastic:

fails to compile:
```json
{
        "type" : "painless_error",
        "reason" : "The maximum number of statements that can be executed in a loop has been reached."
      }
```
this indicates the need to consider writing a plugin script in pure Java to cope with extraction of `SoapAction` from the SOAP XMP payload.


### TODO

  * add exception handler - streamed XML parsing is using recursion and is prone to stack overflows
  * add globbing the contents of the `result['SOAP-ENV:Envelope']['SOAP-ENV:Body']` which is entirely context agnostic, to get some heuristic to get SOAPAction  of a real heavy SOAP XML payloads. Alternatively one can stop right at this level and let ElasticSearch explore the resulting JSON
  * it appears that moving the instantiation of the result `var jsonRes = {}` into the constructor leads to the following damage to the resulting object:
```sh
node ./app.js
```
```text
tag: "SOAP-ENV:Envelope"
tag: "SOAP-ENV:Body"
tag: "m:MyMessage"
tag: "m:MyArgument"
<ref *1> {
  'm:MyArgument': 'Hello',
  'm:MyMessage': [Circular *1],
  'SOAP-ENV:Body': [Circular *1],
  'SOAP-ENV:Envelope': [Circular *1]
}
<ref *1> {
  'm:MyArgument': 'Hello',
  'm:MyMessage': [Circular *1],
  'SOAP-ENV:Body': [Circular *1],
  'SOAP-ENV:Envelope': [Circular *1]
}
```
 * support empty XML elements
```XML
<element/>
```
which currrently is leading to the exception (in Javascript version)
```text
tag: "m:MyEmptyElement/"
Error "RangeError: Maximum call stack size exceeded": Failed to parse the XML.
```
easiest appears to to create and insert closing tags early during the `sanitize`


### See Also

   * another pure JS XML parser [repo](https://github.com/SummersRemote/xmlToJSON) (failing test)
   * another pure js XML DOM parser [repository](https://github.com/iazrael/xmlparser) - untested
   * another fast and simple streaming XML Parser for parsing xml to json objects [repository](https://github.com/Ahmadreza-s/xmlparser) - heavily Regex based, has useful functions to borrow
   * [discussion](https://codereview.stackexchange.com/questions/28307/implementing-an-algorithm-that-walks-the-dom-without-recursion) and [another](https://stackoverflow.com/questions/6969604/recursion-down-dom-tree) of non-recursive DOM tree traversal in Javascript
   
   * Painless ElasticSearch Scripting Language
     + [Script Examples](https://www.elastic.co/guide/en/elasticsearch/painless/7.0/painless-examples.html)
     + [Language Specification](https://www.elastic.co/guide/en/elasticsearch/painless/current/painless-lang-spec.html) - painless scripting language is rather simple but obscure
     + [Painless API Reference](https://www.elastic.co/guide/en/elasticsearch/painless/current/painless-api-reference.html)
     + [Java Classes exposed to Painless](https://www.elastic.co/guide/en/elasticsearch/painless/current/painless-api-reference-shared.html) a.k.a. Shared API
     + [Functions](https://www.elastic.co/guide/en/elasticsearch/painless/current/painless-functions.html)
     + [Walkthrough](https://www.elastic.co/guide/en/elasticsearch/painless/current/painless-walkthrough.html) - shows example using regex to replace the matches in a string
     + [Custom ScriptEngine](https://www.elastic.co/guide/en/elasticsearch/reference/current/modules-scripting-engine.html#modules-scripting-engine)
     + [How To Script Painless-ly in Elasticsearch](https://www.compose.com/articles/how-to-script-painless-ly-in-elasticsearch/)
     + [old repository](https://github.com/rmuir/Painless) from 2015
     + [Circuit Breaker Settings](https://www.elastic.co/guide/en/elasticsearch/reference/current/circuit-breaker.html)
     + [repository](https://github.com/elastic/elasticsearch/tree/main/plugins/examples/script-expert-scoring) with example plugin implementing the interface `ScriptPlugin` and include a [test suite](https://github.com/elastic/elasticsearch/blob/main/plugins/examples/script-expert-scoring/src/yamlRestTest/java/org/elasticsearch/example/expertscript/ExpertScriptClientYamlTestSuiteIT.java) that is required for including in ElasticSearch

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
