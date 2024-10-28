control 'google-compute-firewall-allow-http' do
  impact 1.0
  title 'Ensure HTTP and HTTPS traffic is allowed for the specified firewall rule'
  desc 'Check that the firewall rule allows HTTP (port 80) and HTTPS (port 443) ingress traffic over TCP.'

  firewall_name = 'allow-http-https'
  project_id = 'spheric-alcove-430818-f9'

  describe google_compute_firewall(project: project_id, name: firewall_name) do
    it { should exist }
    its('direction') { should cmp 'INGRESS' }
    its('disabled') { should cmp false }
    its('source_ranges') { should include '0.0.0.0/0' }
    its('target_tags') { should include 'http-server' }

    # Assign the allowed rules to a new symbol and print its type and methods for inspection
    let(:allowed_rules) { subject.allowed }

    let(:allowed_rule_first) { allowed_rules[0] }
    it 'prints the type and methods of allowed_rules' do
      puts "Type of allowed_rules: #{allowed_rules.class}"
      #puts "Methods available on allowed_rules: #{allowed_rules.methods.sort}"
      puts "Type of allowed_rule_first: #{allowed_rule_first.class} #{allowed_rule_first.to_s}"
      puts "Methods available on allowed_rules[0]: #{allowed_rule_first.methods.sort}"
allowed_rule_first.instance_variables.each do |var|
        puts "#{var}: #{allowed_rule_first.instance_variable_get(var)}"
      end
      expect(allowed_rules).not_to be_empty
    end

    # Check if there is an 'allowed' rule with protocol 'tcp' and ports '80' and '443'
    it 'should allow TCP traffic on ports 80 and 443' do
      tcp_rule = allowed_rules.find { |rule| rule.ip_protocol == 'tcp' }
      expect(tcp_rule).not_to be_nil
      expect(tcp_rule.ports).to include('80', '443')
    end
  end
end
