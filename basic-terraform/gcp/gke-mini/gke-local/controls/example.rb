control 'gcloud-node-pool-validation' do
  title 'Validate GKE Node Pool using gcloud command'

# note gcloud command takes different arguments for regional and zonal node pools/clusters
# describe command('gcloud container node-pools describe minimal-node-pool --cluster=minimal-gke --region=us-central1 --format=json') do

  zone_name = attribute('zone_name', description: 'zonal  cluster zone name')
  # https://cloud.google.com/sdk/gcloud/reference/container/node-pools/describe
  describe 'run gcloud container node-pools describe [pool]' do
    subject { command("gcloud container node-pools describe minimal-node-pool --cluster=minimal-gke --zone=#{zone_name} --format=json") }
    its('exit_status') { should eq 0 }
    its('stdout') { should_not be_empty }
    its('stderr') { should be_empty }

    let(:parsed_output) { JSON.parse(subject.stdout) }

    it 'has the correct node count' do
      expect(parsed_output['initialNodeCount']).to eq(1)
    end

    it 'is using preemptible nodes' do
      expect(parsed_output['config']['preemptible']).to be true
    end

    it 'has the expected machine type' do
      expect(parsed_output['config']['machineType']).to eq('e2-micro')
    end
  end
end


