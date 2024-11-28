### Info

```
inspec exec . -t local://  --input-file=attributes.yml 
```
```
One of [--location, --zone, --region] must be supplied.
```
```
✔  gcloud-node-pool-validation: Validate GKE Node Pool using gcloud command
      ✔  run gcloud container node-pools describe [pool] has the correct node count
      ✔  run gcloud container node-pools describe [pool] is using preemptible nodes
      ✔  run gcloud container node-pools describe [pool] has the expected machine type
      ✔  run gcloud container node-pools describe [pool] exit_status is expected to eq 0
      ✔  run gcloud container node-pools describe [pool] stdout is expected not to be empty
      ✔  run gcloud container node-pools describe [pool] stderr is expected to be empty

```

### Cons and Pros

* as long as inspec shell is not working with `inspec-gcp`, the only way to add property tests is through inspecting the JSON. 
* with complex `gcloud GROUP | COMMAND` commandss e.g. https://cloud.google.com/sdk/gcloud/reference/notebooks/instances/describe 
there is no matching  Inspec GCP resource https://github.com/inspec/inspec-gcp/tree/main/docs/resources