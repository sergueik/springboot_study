```
title "Ensure Container-Optimized OS (COS) is used for GKE node images"


    clusters.each do |cluster|
      google_container_node_pools(
        project: gcp_project_id, 
        location: gke_cluster[:location], 
        cluster_name: gke_cluster[:cluster_name]).node_pool_names.each do |node_pool_name|
        describe "Node Pool: #{node_pool_name}" do
          subject { google_container_node_pool(
                      project: gcp_project_id, 
                      location: gke_cluster[:location], 
                      cluster_name: gke_cluster[:cluster_name], 
                      nodepool_name: node_pool_name) }
          its('config.image_type') { should match(/COS/) }
        end
      end
    end

```

```
  describe 'validating Kubernetes node status' do
    subject do
        command('kubectl get nodes -o jsonpath="{.items[*].status.nodeInfo}"')
    end
    it 'should have Container-Optimized OS image' do
      # Load the stdout (JSON string) and parse it
      parsed_json = JSON.parse(subject.stdout)
      os_image = parsed_json.dig(0, 'osImage') 
      expect(os_image).to eq('Container-Optimized OS from Google')  
      end
    end
  end     
```