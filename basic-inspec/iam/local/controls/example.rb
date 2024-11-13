# frozen_string_literal: true

title 'IAM Test'

gcp_project = attribute('project_name', description: 'GCP Project Name')
service_account_email = attribute('email', description: 'Service Account Email')


# Skipping profile: 'inspec-gcp-deep-dive-profile' on unsupported platform: 'windows_8.1/6.3.9600'.
control "local" do
  # https://docs.chef.io/inspec/resources/command/
  describe command("gcloud projects get-iam-policy #{attribute('project_id')} --flatten='bindings[].members' --filter='bindings.role:roles/iam.securityReviewer AND bindings.members:serviceAccount:#{service_account_email}' --format='json'") do
    its('exit_status') { should eq 0 }
    its('stdout') { should_not eq '[]' }  # Ensure that the role is attached
  end

  describe command("gcloud projects get-iam-policy #{attribute('project_id')} --flatten='bindings[].members' --filter='bindings.role:roles/compute.admin' --format='json'") do
    let(:json_output) { JSON.parse(subject.stdout) }
    let(:known_service_account) {"serviceAccount:#{service_account_email}"} 
    it 'should contain the known service account in bindings' do
    ## https://docs.ruby-lang.org/en/3.1/dig_methods_rdoc.html
      all_members = json_output.flat_map { |entry| entry.dig('bindings', 'members') }.compact
      expect(all_members).to include(known_service_account)
    end
    # result.class.name.split('::').last
  end

end

require 'csv'

control 'csv-output-test' do
  describe command("gcloud asset search-all-iam-policies --scope='projects/spheric-alcove-430818-f9' --query='roles:roles/iam.serviceAccountTokenCreator'  --format='csv(policy.bindings.members,policy.bindings.role)' --flatten='policy.bindings[].members' ").stdout do
    let(:parsed_output) { CSV.parse(subject, headers: true) }

    it 'should have more than one row' do
      expect(parsed_output.size).to be > 1
    end

    it 'should contain specific data' do
      # user:kouzmine.serguei@gmail.com,roles/iam.serviceAccountTokenCreator
      expect(parsed_output.any? { |row| row['members'] == 'user:kouzmine.serguei@gmail.com' }).to be true
    end
  end
end
