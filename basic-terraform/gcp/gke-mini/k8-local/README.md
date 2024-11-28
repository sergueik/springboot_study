```

gcloud projects add-iam-policy-binding spheric-alcove-430818-f9   --member="user:kouzmine.serguei@gmail.com"   --role="roles/container.viewer"
gcloud projects add-iam-policy-binding spheric-alcove-430818-f9   --member="user:kouzmine.serguei@gmail.com"   --role="roles/container.developer"
gcloud container clusters get-credentials minimal-gke --zone us-central1-c --project  spheric-alcove-430818-f9
export KUBECONFIG=~/.kube/config
```

```
inspec exec . -t local:// --input-file attributes.yml
```
