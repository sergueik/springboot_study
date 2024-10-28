# frozen_string_literal: true

title 'Single zone test'
bucket_name = attribute('bucket_name', description: 'Bucket Name')

gcp_project = attribute('project_name', description: 'GCP Project Name')
gcp_zone = attribute('zone_name', description: 'GCP zone name to test')

describe google_storage_bucket(name: 
bucket_name) do
  it { should exist }
  its('location') { should cmp gcp_zone.upcase }

  its('storage_class') { should eq "STANDARD" }
 # its('labels') { should include("key" => "value") }
  its('retention_policy.retention_period') { should cmp 1000 }
end
