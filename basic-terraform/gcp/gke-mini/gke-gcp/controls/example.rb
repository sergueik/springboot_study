# InSpec control to verify a specific GKE node pool
control 'gke-specific-node-pool' do
  title 'Verify Specific GKE Node Pool'

  zone_name = attribute('zone_name', description: 'zonal cluster zone name')
  # https://github.com/inspec/inspec-gcp/blob/main/docs/resources/google_container_node_pools.md
  google_container_node_pools(
    project: 'spheric-alcove-430818-f9', 
    location: zone_name, 
    cluster_name: 'minimal-gke'
    ).node_pool_names.each do |nodepoolname|

    describe "google_container_node_pool resource #{nodepoolname}" do
      # https://github.com/inspec/inspec-gcp/blob/main/docs/resources/google_container_node_pool.md 
      subject {google_container_node_pool(
        project: 'spheric-alcove-430818-f9',
        location: zone_name,
        cluster_name: 'minimal-gke',
        nodepool_name: nodepoolname
      )
      }
      it { should exist }
      # its('node_count') { should cmp 1 }  # Ensure only 1 node
      its('initial_node_count'){should eq 1}

      its('config.preemptible') { should be true } # Ensure nodes are preemptible
      its('config.machine_type') { should cmp 'e2-micro' } # Check machine type
      its('config.image_type') { should match(/COS/) }
      # 
      its('management.auto_upgrade' ) { should be false }  
    end
  end
  # https://github.com/inspec/inspec-gcp/blob/main/docs/resources/google_container_clusters.md
  describe google_container_clusters(project: 'spheric-alcove-430818-f9', location: zone_name) do
    its('cluster_names') { should include 'minimal-gke' }
    its('node_configs')  { should_not be_empty } # toggle to should be_empty to see the object inside
    its('node_configs.length')  { should eq 1 } 
    its('node_configs.first.metadata')  { should have_key('disable-legacy-endpoints') } 


     # Access the metadata of the first node config
     it 'checks metadata in the first node config' do
      node_config = subject.node_configs.first
      # https://www.geeksforgeeks.org/ruby-array-class-first-function/
      expect(node_config.metadata).to include('disable-legacy-endpoints' => 'false')
      # attempt  to trigger failure
      # will see the
      # Diff:
      # @@ -1 +1 @@
      # -"disable-legacy-endpoints" => "false",
     # +"disable-legacy-endpoints" => "true", 
    end

    it 'prints the public methods and properties of the resource' do
      node_config = subject.node_configs.first
      puts "Properties:" 
      node_config.instance_variables.each do |var|
        puts "#{var}: #{node_config.instance_variable_get(var)}"
      end
    end
    
   end
end
