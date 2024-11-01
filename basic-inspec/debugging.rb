# Assuming you are testing an InSpec resource, you can print its methods and properties by calling the public_methods or instance_variables on the resource.
control 'example' do
  describe google_compute_instance(project: 'my_project', zone: 'us-central1-a', name: 'my-instance') do
    it 'prints the public methods and properties of the resource' do
      # Print all public methods
      puts "Public Methods: #{subject.public_methods(false)}"
      
      # Print instance variables (properties)
      puts "Properties: #{subject.instance_variables}"

      # Print method values (fetch dynamically)
      subject.public_methods(false).each do |method|
        puts "#{method}: #{subject.send(method)}" if subject.respond_to?(method)
      end
    end
      it 'prints all public methods' do
      puts subject.methods.sort
    end

  end      
