# frozen_string_literal: true

title 'Single zone test'
bucket_name = attribute('bucket_name', description: 'Bucket Name')
object_name = attribute('object_name', description: 'Object Name')

gcp_project = attribute('project_name', description: 'GCP Project Name')
gcp_zone = attribute('zone_name', description: 'GCP zone name to test')

# https://github.com/inspec/inspec-gcp/blob/main/docs/resources/google_storage_bucket.md
describe google_storage_bucket(name:  bucket_name) do
  it { should exist }
  its('location') { should cmp gcp_zone.upcase }

  # https://cloud.google.com/storage/docs/storage-classes#standard
  its('storage_class') { should eq "STANDARD" }
  its('labels') { should include("key" => "value") }
  # NOTE: typo lead to undefined method `main_page_suffix' for nil:NilClass
  its('website.main_page_suffix') { should cmp  "index.html" }

 # its('retention_policy.retention_period') { should cmp 1000 }
end

# https://github.com/inspec/inspec-gcp/blob/main/docs/resources/google_storage_bucket_object.md
describe google_storage_bucket_object(bucket: bucket_name, object: object_name) do
  it { should exist }
  its('size.to_i') { should be > 10 }
   its('content_type') { should eq "text/plain; charset=utf-8" }
  its('time_created') { should be > Time.now - 60*10 }
  its('md5_hash') { should eq 'abe45d28281cfa2a4201c9b90a143095' }
# expected: "abe45d28281cfa2a4201c9b9a14395"
 #         got: "q+RdKCgc+ipCAcm5ChQwlQ=="
end
