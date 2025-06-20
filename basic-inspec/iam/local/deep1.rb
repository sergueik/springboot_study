control 'deeply-nested-json-test' do
  describe json(command('your-json-output-command').stdout) do
    let(:parsed_output) { subject.params }

    it 'should have at least 2 entries with matching filenames in the nested structure' do
      # Flatten and collect all details into a single array
      all_details = parsed_output['data'].flat_map do |data_entry|
        data_entry.dig('info', 'details') || []
      end

      # Filter entries based on the Filename property
      filtered_entries = all_details.select { |entry| entry['Filename'] =~ /file.*\.txt/ }

      # Check the count of filtered entries
      expect(filtered_entries.size).to be >= 2
    end
  end
end
Explanation:
