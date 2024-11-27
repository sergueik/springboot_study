```
export GOOGLE_APPLICATION_CREDENTIALS=~/my-profile/keys.json
inspec exec . --input-file=attributes.yml -t gcp://
```


```



 ×  gke-specific-node-pool: Verify Specific GKE Node Pool (1 failed)
     ✔  google_container_node_pool resource is expected to exist
     ✔  google_container_node_pool resource initial_node_count is expected to eq 1
     ✔  google_container_node_pool resource config.preemptible is expected to equal true
     ✔  google_container_node_pool resource config.machine_type is expected to cmp == "e2-micro"
     ✔  google_container_node_pool resource config.image_type is expected to match /COS/
     ×  google_container_node_pool resource management.auto_upgrade is expected to equal false
     
     expected false
          got true

     ✔  google_container_clusters cluster_names is expected to include "minimal-gke"

Profile:   Google Cloud Platform Resource Pack (inspec-gcp)
Version:   1.11.135
Target:    gcp://terraform-with-gcp@spheric-alcove-430818-f9.iam.gserviceaccount.com
Target ID: beabb9d4-4fc2-552b-a6be-3820a467f89c

     No tests executed.

```

### Exploring 

```
×  google_container_clusters node_configs is expected to be empty
     expected `[#<#<Class:0x000078166d7fc460>::GoogleInSpec::Container::Property::ClusterNodeConfig:0x000078165f8d93...container_clusters ClusterNodeConfig", @enable_secure_boot=nil, @enable_integrity_monitoring=true>>].empty?` to be truthy, got fals

```
