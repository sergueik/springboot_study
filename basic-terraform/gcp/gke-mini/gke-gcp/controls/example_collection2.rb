# Aggregate Results Across All Clusters and one test
control 'gke-all-cluster-node-config-metadata' do
  title 'Verify node config metadata across all GKE clusters'

  clusters = google_container_clusters(project: 'spheric-alcove-430818-f9', location: zone_name).cluster_names
  # https://www.geeksforgeeks.org/ruby-array-select-function/
# https://www.geeksforgeeks.org/ruby-enumerable-flat_map-function/
  all_matching_node_configs = clusters.flat_map do |cluster_name|
    google_container_cluster(project: 'spheric-alcove-430818-f9', location: zone_name, name: cluster_name)
      .node_configs
      .select { |node_config| node_config.metadata['disable-legacy-endpoints'] == 'true' }
  end

  describe 'Node configs with matching metadata' do
    it 'should exist across all clusters' do
      expect(all_matching_node_configs).not_to be_empty
    end
  end
end
