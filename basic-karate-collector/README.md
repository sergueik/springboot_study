### Info

replica of [basic Karate tesst](https://github.com/KaterinaUK/automated_API_with_Karate)
with `https://jsonplaceholder.typicode.com/` used as system under test.

### Usage

* run with maven (standalone run and run with gradle are alternatives)
```sh
mvn clean test
```
* after successful run there is a  summary report  `target\karate-reports\karate-summary-json.txt` in JSON format
```JSON
{
  "efficiency": 0.22719805728162273,
  "totalTime": 3181,
  "threads": 1,
  "resultDate": "2023-02-08 10:19:52 PM",
  "env": null,
  "version": "1.3.1",
  "scenariosfailed": 0,
  "featureSummary": [
    {
      "failedCount": 0,
      "packageQualifiedName": "example.feature.jsonplaceholder"
      "relativePath": "example/feature/jsonplaceholder.feature"
      "scenarioCount": 2,
      "name": "Tests for the json placeholder page",
      "description": "",
      "durationMillis": 3180.838618,
      "passedCount": 2,
      "failed": false
    }
  ],
  "featuresPassed": 1,
  "featuresFailed": 0,
  "featuresSkipped": 0,
  "scenariosPassed": 2,
  "elapsedTime": 14001
}
```
and HTML page
![success](https://github.com/sergueik/springboot_study/blob/master/basic-karate-collector/screenshots/capture-success.png)

* after failed run
```JSON
{
  "efficiency": 0.25334247514569763,
  "totalTime": 2956,
  "threads": 1,
  "resultDate": "2023-02-08 10:25:46 PM",
  "env": null,
  "version": "1.3.1",
  "scenariosfailed": 1,
  "featureSummary": [
    {
      "failedCount": 1,
      "packageQualifiedName": "example.feature.jsonplaceholder",
      "relativePath": "example/feature/jsonplaceholder.feature",
      "scenarioCount": 2,
      "name": "Tests for the json placeholder page",
      "description": "",
      "durationMillis": 2956.155631,
      "passedCount": 1,
      "failed": true
    }
  ],
  "featuresPassed": 0,
  "featuresFailed": 1,
  "featuresSkipped": 0,
  "scenariosPassed": 1,
  "elapsedTime": 11668
}
```
![failed run](https://github.com/sergueik/springboot_study/blob/master/basic-karate-collector/screenshots/capture-failure.png)

* the details are in  `target\karate-reports\example.feature.jsonplaceholder.karate-json.txt`:
```JSON
{
  "failedCount": 1,
  "prefixedPath": "classpath:example/feature/jsonplaceholder.feature",
  "packageQualifiedName": "example.feature.jsonplaceholder",
  "loopIndex": -1,
  "relativePath": "example/feature/jsonplaceholder.feature",
  "scenarioResults": [
    {
      "sectionIndex": 0,
      "stepResults": [
        {
          "result": {
            "nanos": 5950378,
            "millis": 5.950378,
            "status": "passed"
          },
          "step": {
            "background": true,
            "line": 4,
            "prefix": "Given",
            "index": 0,
            "text": "url 'https://jsonplaceholder.typicode.com/'"
          },
          "stepLog": "22:25:40.858 karate.env system property was: null \n"
        },
        {
          "result": {
            "nanos": 3020092,
            "millis": 3.020092,
            "status": "passed"
          },
          "step": {
            "line": 7,
            "prefix": "Given",
            "index": 0,
            "text": "path 'todos/1'"
          }
        },
        {
          "result": {
            "nanos": 2599152219,
            "millis": 2599.152219,
            "status": "passed"
          },
          "step": {
            "line": 8,
            "prefix": "When",
            "index": 1,
            "text": "method Get"
          }
        },
        {
          "result": {
            "nanos": 38489,
            "millis": 0.038489,
            "status": "passed"
          },
          "step": {
            "line": 9,
            "prefix": "Then",
            "index": 2,
            "text": "status 200"
          }
        },
        {
          "result": {
            "nanos": 116111479,
            "millis": 116.111479,
            "status": "passed"
          },
          "step": {
            "line": 10,
            "prefix": "And",
            "index": 3,
            "text": "match response.userId == 1"
          }
        },
        {
          "result": {
            "nanos": 3928429,
            "millis": 3.928429,
            "status": "passed"
          },
          "step": {
            "line": 11,
            "prefix": "And",
            "index": 4,
            "text": "match response.title == '#string'"
          }
        }
      ],
      "executorName": "main",
      "line": 6,
      "name": "Get todo",
      "description": "",
      "durationMillis": 2728.201086,
      "startTime": 1675913139926,
      "failed": false,
      "refId": "[1:6]",
      "endTime": 1675913143849,
      "exampleIndex": -1
    },
    {
      "sectionIndex": 1,
      "stepResults": [
        {
          "result": {
            "nanos": 1405271,
            "millis": 1.405271,
            "status": "passed"
          },
          "step": {
            "background": true,
            "line": 4,
            "prefix": "Given",
            "index": 0,
            "text": "url 'https://jsonplaceholder.typicode.com/'"
          },
          "stepLog": "22:25:43.899 karate.env system property was: null \n"
        },
        {
          "result": {
            "nanos": 933569,
            "millis": 0.933569,
            "status": "passed"
          },
          "step": {
            "line": 15,
            "prefix": "Given",
            "index": 0,
            "text": "path 'todos'"
          }
        },
        {
          "result": {
            "nanos": 213293710,
            "millis": 213.29371,
            "status": "passed"
          },
          "step": {
            "line": 16,
            "prefix": "When",
            "index": 1,
            "text": "method Get"
          },
        },
        {
          "result": {
            "nanos": 22238,
            "millis": 0.022238,
            "status": "passed"
          },
          "step": {
            "line": 17,
            "prefix": "Then",
            "index": 2,
            "text": "status 200"
          }
        },
        {
          "result": {
            "nanos": 6248453,
            "millis": 6.248453,
            "status": "passed"
          },
          "step": {
            "line": 18,
            "prefix": "And",
            "index": 3,
            "text": "match response[0].title == '#string'"
          }
        },
        {
          "result": {
            "nanos": 6051304,
            "errorMessage": "match failed: EQUALS\n  $ | not equal (NUMBER:NUMBER)\n  1\n  0\n\nclasspath:example/feature/jsonplaceholder.feature:19",
            "millis": 6.051304,
            "status": "failed"
          },
          "step": {
            "line": 19,
            "prefix": "And",
            "index": 4,
            "text": "match response[0].userId == 0"
          },
          "stepLog": "22:25:44.121 classpath:example/feature/jsonplaceholder.feature:19\nAnd match response[0].userId == 0\nmatch failed: EQUALS\n  $ | not equal (NUMBER:NUMBER)\n  1\n  0\n\nclasspath:example/feature/jsonplaceholder.feature:19\n"
        }
      ],
      "line": 14,
      "description": "",
      "durationMillis": 227.954545,
      "failed": true,
      "error": "match failed: EQUALS\n  $ | not equal (NUMBER:NUMBER)\n  1\n  0\n\nclasspath:example/feature/jsonplaceholder.feature:19",
      "executorName": "main",
      "name": "List todos",
      "startTime": 1675913143888,
      "refId": "[2:14]",
      "endTime": 1675913144121,
      "exampleIndex": -1
    }
  ],
  "callDepth": 0,
  "name": "Tests for the json placeholder page",
  "description": "",
  "resultDate": "2023-02-08 10:25:44 PM",
  "durationMillis": 2956.155631,
  "passedCount": 1
}

```
the `"stepLog"` contains actual exhange payload, and the log file can grow quite big

### See Also

  * https://github.com/fertekton/karate-api/blob/main/src/test/java/Yevo/BackEndTests.java

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
