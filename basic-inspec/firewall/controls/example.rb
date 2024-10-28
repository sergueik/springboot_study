control 'google-compute-firewall-allow-http' do
  impact 1.0
  title 'Ensure HTTP and HTTPS traffic is allowed for the specified firewall rule'
  desc 'Check that the firewall rule allows HTTP (port 80) and HTTPS (port 443) ingress traffic over TCP.'

  # Replace these values with your actual project ID and firewall rule name
  firewall_name = 'allow-http-https'
  project_id = 'spheric-alcove-430818-f9'

  describe google_compute_firewall(project: project_id, name: firewall_name) do
    it { should exist }
    its('direction') { should cmp 'INGRESS' }
    its('disabled') { should cmp false }
    its('source_ranges') { should include '0.0.0.0/0' }
    its('target_tags') { should include 'http-server' }

    # Check that the firewall rule allows TCP traffic on ports 80 and 443
    it 'should allow TCP traffic on ports 80 and 443' do
      expect(subject.allowed).not_to be_empty
      expect(subject.allowed.map { |rule| rule['protocol'] }).to include 'tcp'
      expect(subject.allowed.map { |rule| rule['ports'] }.flatten).to include('80', '443')
    end
  end
end
