# InSpec control to verify a specific GKE node pool
control 'gke-specific-node-pool' do
  title 'Verify Specific GKE Node Pool'

  describe google_container_node_pool(
    project: 'spheric-alcove-430818-f9',
    location: 'us-central1',
    cluster_name: 'minimal-gke',
    nodepool_name: 'minimal-node-pool'
  ) do
    it { should exist }
    # its('node_count') { should cmp 1 }  # Ensure only 1 node
    its('initial_node_count'){should eq 3}

    its('config.preemptible') { should be true } # Ensure nodes are preemptible
    its('config.machine_type') { should cmp 'e2-micro' } # Check machine type
    its('config.image_type') { should match(/COS/) }
  end

  describe google_container_clusters(project: 'spheric-alcove-430818-f9', location: 'us-central1') do
  its('cluster_names') { should include 'minimal-gke' }
  end
end
