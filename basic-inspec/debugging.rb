control 'example' do
  describe google_compute_instance(project: 'my_project', zone: 'us-central1-a', name: 'my-instance') do
    it 'prints the public methods and properties of the resource' do
      # Print all public methods
      puts "Public Methods: #{subject.public_methods(false)}"

      # Print instance variables (properties) and their values
      puts "Properties:"
      subject.instance_variables.each do |var|
        puts "#{var}: #{subject.instance_variable_get(var)}"
      end

      # Print method values (fetch dynamically)
      subject.public_methods(false).each do |method|
        puts "#{method}: #{subject.send(method)}" if subject.respond_to?(method)
      end
    end

    # A normal test example
    it { should exist }
    its('status') { should cmp 'RUNNING' }
  end
end

