# frozen_string_literal: true

title 'Single zone test'

gcp_project = attribute('project_name', description: 'GCP Project Name')
gcp_zone = attribute('zone_name', description: 'A valid GCP zone name to test')

describe google_project(project: gcp_project) do
  it { should exist }
  # its('project_id') { should cmp 'chef-gcp-inspec' }
  its('lifecycle_state') { should cmp 'ACTIVE' }
end

control 'gcp-single-zone-1' do
  impact 1.0
  title 'Check the status of a single zone'
  describe google_compute_zone(project: gcp_project, name: gcp_zone) do
    it { should exist }
    its('status') { should eq 'UP' }
  end
end


control "gcp" do
  impact 1.0

describe google_service_account(project: attribute("project_id"), name: attribute("email")) do
  it { should exist }
    its('project_id') { should eq attribute('project_id') }
  end

  describe google_project_iam_binding(project: "#{attribute("project_id")}",  role: 'roles/viewer') do
    its('members') {should include attribute('iam_email') }
  end

  describe google_project_iam_binding(project: "#{attribute("project_id")}",  role: 'roles/iam.securityReviewer') do
    its('members') {should include attribute('iam_email') }
  end

describe google_service_account(project: attribute("project_id"), name: attribute("email")) do
 it { should exist }
  its('display_name') { should cmp 'terraform-with-gcp'}
  end
end
