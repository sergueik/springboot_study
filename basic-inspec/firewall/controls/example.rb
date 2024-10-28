# frozen_string_literal: true

title 'firewall test'
bucket_name = attribute('bucket_name', description: 'Bucket Name')
object_name = attribute('object_name', description: 'Object Name')

project_name = attribute('project_name', description: 'GCP Project Name')
zone_name = attribute('zone_name', description: 'GCP zone name to test')
control "firewall" do
  impact 1.0

  title 'Verify allow and deny rules in Google Compute firewall'
  desc 'Ensure that the Google Compute firewall has correct allow and deny rules configured'

  firewall_name = 'allow-http-https'
  project = 'spheric-alcove-430818-f9'

  # Fetch the firewall resource once
  firewall = google_compute_firewall(project: project, name: firewall_name)

  describe firewall do
    it { should exist }
  end

  # Refactored tests for allow rules
  if firewall.allowed
    describe 'allow rules' do
      subject { google_compute_firewall(project: project, name: firewall_name).allowed }
      
      it { should_not be_empty }
      its('ports.flatten') { should include '80' }
      its('IPProtocol') { should include 'tcp' }
      # Add further allow rule checks as needed
    end

  end

end