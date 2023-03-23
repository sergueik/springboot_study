#!/bin/sh

# based on: quicklab #1 in 
# "Developing Applications with Cloud Functions on Google Cloud"
# https://app.pluralsight.com/library/courses/developing-applications-cloud-functions-google-cloud
DATA=$(cat <<EOF

{
  "data": {
    "rows": [
      {
        "name": "Ray",
        "message": "Hello",
        "imageUri": null,
        "_links": {
          "self": {
            "href": "http://localhost:8081/rows/1"
          },
          "link": {
            "href": "http://localhost:8081/rows/1"
          }
        }
      },
      {
        "name": "Charles",
        "message": "Hello",
        "imageUri": null,
        "_links": {
          "self": {
            "href": "http://localhost:8081/rows/1"
          },
          "link": {
            "href": "http://localhost:8081/rows/1"
          }
        }
      }
    ]
  }
}


EOF
)

echo $DATA
