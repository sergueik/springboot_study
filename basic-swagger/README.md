
### Info
https://github.com/bennzhang/spring-boot-swagger2-demo

```json
{
  "swagger": "2.0",
  "info": {
    "description": "Spring Boot Swagger Example Api Creator",
    "version": "1.0",
    "title": "Spring Boot Swagger Example API",
    "termsOfService": "Terms of Service",
    "contact": {
      "name": "yourname",
      "url": "https://github.com/yourlink",
      "email": "youremail@yourdomain.com"
    },
    "license": {
      "name": "Apache License Version 2.0",
      "url": "https://www.apache.org/licesen.html"
    }
  },
  "host": "localhost:8080",
  "basePath": "/",
  "tags": [
    {
      "name": "person-resource",
      "description": "Shows the user info"
    }
  ],
  "paths": {
    "/rest/person/{userName}": {
      "get": {
        "tags": [
          "person-resource"
        ],
        "summary": "getPerson",
        "operationId": "getPersonUsingGET",
        "consumes": [
          "application/json"
        ],
        "produces": [
          "*/*"
        ],
        "parameters": [
          {
            "name": "userName",
            "in": "path",
            "description": "userName",
            "required": true,
            "type": "string"
          }
        ],
        "responses": {
          "200": {
            "description": "OK",
            "schema": {
              "$ref": "#/definitions/Person"
            }
          },
          "401": {
            "description": "Unauthorized"
          },
          "403": {
            "description": "Forbidden"
          },
          "404": {
            "description": "Not Found"
          }
        }
      }
    }
  },
  "definitions": {
    "Person": {
      "type": "object",
      "properties": {
        "name": {
          "type": "string",
          "description": "name"
        },
        "salary": {
          "type": "integer",
          "format": "int64",
          "description": "salary"
        },
        "title": {
          "type": "string",
          "description": "title"
        }
      }
    }
  }
}

```


### See Also

  * [Generate Spring Boot REST Client project with Swagger](https://www.baeldung.com/spring-boot-rest-client-swagger-codegen)
  o [Generate Spring Boot Project with Swagger]()
  * [Setting Up Swagger 2 with a Spring REST API](https://www.baeldung.com/swagger-2-documentation-for-spring-rest-api)
  * [reading Swagger spec from YAML file](https://qna.habr.com/q/1080992)(in Russian)
 * https://swagger.io/docs/swagger-inspector/how-to-create-an-openapi-definition-using-swagger/
 * https://www.baeldung.com/swagger-2-documentation-for-spring-rest-api
 * https://stackoverflow.com/questions/36745620/swagger-2-spring-boot-generate-yml-file
 * https://howtodoinjava.com/swagger2/swagger-spring-mvc-rest-example/