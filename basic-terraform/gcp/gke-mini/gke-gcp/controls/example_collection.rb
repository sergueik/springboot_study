control 'gke-cluster-node-config-metadata' do
  title 'Verify node config metadata for all GKE clusters'

  google_container_clusters(project: 'spheric-alcove-430818-f9', location: zone_name).cluster_names.each do |cluster_name|
    describe google_container_cluster(project: 'spheric-alcove-430818-f9', location: zone_name, name: cluster_name) do
      it { should exist }

      it "checks if any node config in cluster #{cluster_name} has specific metadata" do
        # Filter node configs that include specific metadata
        matching_node_configs = subject.node_configs.select do |node_config|
          node_config.metadata['disable-legacy-endpoints'] == 'true'
        end

        # Expect at least one matching node config
        expect(matching_node_configs).not_to be_empty
      end
    end
  end
end
