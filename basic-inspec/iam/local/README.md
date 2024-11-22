# Example InSpec Profile
```
export GOOGLE_APPLICATION_CREDENTIALS=~/my-profile/keys.json
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

command to use with console tests
```shh
gcloud asset search-all-iam-policies --scope='projects/spheric-alcove-430818-f9' --query='roles:roles/iam.serviceAccountTokenCreator'  --format='csv(policy.bindings.members,policy.bindings.role)' 
--flatten='policy.bindings[].members' 
```