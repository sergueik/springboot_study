# frozen_string_literal: true

title 'IAM Test'

gcp_project = attribute('project_name', description: 'GCP Project Name')
service_account_email = attribute('email', description: 'Service Account Email')


# Skipping profile: 'inspec-gcp-deep-dive-profile' on unsupported platform: 'windows_8.1/6.3.9600'.
control "local" do
  describe command("gcloud projects get-iam-policy #{attribute('project_id')} --flatten='bindings[].members' --filter='bindings.role:roles/iam.securityReviewer AND bindings.members:serviceAccount:#{service_account_email}' --format='json'") do
    its('exit_status') { should eq 0 }
    its('stdout') { should_not eq '[]' }  # Ensure that the role is attached
  end

  describe command("gcloud projects get-iam-policy #{attribute('project_id')} --flatten='bindings[].members' --filter='bindings.role:roles/compute.admin' --format='json'") do
    let(:instance_info) { JSON.parse(subject.stdout) }
    its('stdout') { should eq '[]' }  # uncomment to see the output
    it 'should have a  state' do
      expect(instance_info[0]['bindings']['members']).to include service_account_email
    end
    it 'should have a  state' do
      expect(instance_info[0]['bindings']['members']).not_to eq []
    end
    it 'should have a  state' do
      expect(instance_info[0]['bindings']['members']).to eq []
    end
    its('stdout') { should eq '[]' }  # uncomment to see the output
  end

end


