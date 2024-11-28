```

gcloud projects add-iam-policy-binding spheric-alcove-430818-f9   --member="user:kouzmine.serguei@gmail.com"   --role="roles/container.viewer"
gcloud projects add-iam-policy-binding spheric-alcove-430818-f9   --member="user:kouzmine.serguei@gmail.com"   --role="roles/container.developer"
gcloud container clusters get-credentials minimal-gke --zone us-central1-c --project  spheric-alcove-430818-f9
export KUBECONFIG=~/.kube/config
```

```
inspec exec . -t local:// --input-file attributes.yml
```

```
------------------------------------------------------------
  ! [WARNING] Non-Commercial License

  You are using a free tier version - not meant for commercial usage.

  If you are using it for commercial purposes, please reach
  out to our sales team at chef-sales@progress.com to get
  commercial license to be compliant with Progress Chef MLSA.
------------------------------------------------------------
Project ID: spheric-alcove-430818-f9
Cluster Name: minimal-gke
Zone: us-central1-c
User Email: kouzmine.serguei@gmail.com
Service Account Key: /home/kouzmine_serguei/my-profile/keys.json
Kubeconfig Path: /home/kouzmine_serguei/.kube/config

Profile:   InSpec Profile (local)
Version:   0.1.0
Target:    local://
Target ID: 1ef43cba-d81b-5ca1-9f67-ad744c7e5d30

  ×  k8s-cluster-setup: Ensure Kubernetes Cluster Setup is Correct (1 failed)
     ✔  Debug: Check attributes outputs the project_id
     ✔  Debug: Check attributes outputs the cluster_name
     ✔  Debug: Check attributes outputs the zone
     ✔  Debug: Check attributes outputs the user_email
     ✔  Debug: Check attributes outputs the service_account_key
     ✔  Debug: Check attributes outputs the kubeconfig_path
     ✔  File /home/kouzmine_serguei/my-profile/keys.json is expected to exist
     ✔  File /home/kouzmine_serguei/my-profile/keys.json is expected to be file
     ×  run gcloud auth activate service account exit_status is expected to eq 0
     
     expected: 0
          got: 1
     
     (compared using ==)

     ✔  run gcloud projects add-iam-policy-binding for container.viewer exit_status is expected to eq 0
     ✔  run gcloud projects add-iam-policy-binding for container.developer exit_status is expected to eq 0
     ✔  run gcloud container clusters get-credentials exit_status is expected to eq 0
     ✔  run kubectl get nodes exit_status is expected to eq 0


Profile Summary: 0 successful controls, 1 control failure, 0 controls skipped
Test Summary: 12 successful, 1 failure, 0 skipped
```
