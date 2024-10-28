# Example InSpec Profile
```
inspec exec . -t local:// --input-file attributes.yml
```

This example shows the implementation of an InSpec profile.


dificult to examine json:
```
[
  {
    "bindings": {
      "members": "serviceAccount:spheric-alcove-430818-f9@appspot.gserviceaccount.com",
      "role": "roles/compute.admin"
    },
    "etag": "BwYlfkSHbA0=",
    "version": 1
  },
  {
    "bindings": {
      "members": "serviceAccount:terraform-with-gcp@spheric-alcove-430818-f9.iam.gserviceaccount.com",
      "role": "roles/compute.admin"
    },
    "etag": "BwYlfkSHbA0=",
    "version": 1
  }
]
```