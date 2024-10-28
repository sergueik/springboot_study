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
      it 'should have at least one allow rule' do
        expect(firewall.allowed).not_to be_empty
      end
      it 'should include port 80 in allowed ports' do
        expect(firewall.allowed.map { |rule| rule['ports'] }.flatten).to include '80'
      end
      it 'should allow tcp protocol' do
        expect(firewall.allowed.map { |rule| rule['IPProtocol'] }).to include 'tcp'
      end
    end
  end

  # Refactored tests for deny rules
  if firewall.denied
    describe 'deny rules' do
      it 'should have at least one deny rule' do
        expect(firewall.denied).not_to be_empty
      end
      it 'should include port 22 in denied ports' do
        expect(firewall.denied.map { |rule| rule['ports'] }.flatten).to include '22'
      end
      it 'should deny tcp protocol' do
        expect(firewall.denied.map { |rule| rule['IPProtocol'] }).to include 'tcp'
      end
    end
  end

end