# frozen_string_literal: true

title 'instance test'
bucket_name = attribute('bucket_name', description: 'Bucket Name')
object_name = attribute('object_name', description: 'Object Name')

project_name = attribute('project_name', description: 'GCP Project Name')
zone_name = attribute('zone_name', description: 'GCP zone name to test')
control "instance" do
  impact 1.0

describe google_compute_instance(project: project_name, zone: zone_name, name: 'flask-vm') do
  it { should exist }
  its('machine_type') { should match 'f1-micro' }
  its('tags.items') { should include 'ssh' } 

 its('network_interfaces.count') { should be > 0 }

    # Check that there is at least one access configuration in the first network interface
    it 'should have an access config with a public IP' do
      expect(subject.network_interfaces.first.access_configs).not_to be_empty
      expect(subject.network_interfaces.first.access_configs.first.nat_ip).not_to be_nil
    end
    it 'should have an subnetwork' do
      expect(subject.network_interfaces.first.subnetwork).not_to be_empty
    end
end
end