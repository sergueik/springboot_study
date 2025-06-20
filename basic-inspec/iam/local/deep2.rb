control 'filtered-nested-json-row-count-test' do
  describe json(command('your-json-output-command').stdout) do
    let(:parsed_output) { subject.params['data'] }

    it 'should have at least 2 entries where Filename matches a condition' do
      filtered_rows = parsed_output.select do |entry|
        entry.dig('attributes', 'info', 'Filename') =~ /file.*\.txt/
      end
      expect(filtered_rows.size).to be >= 2
    end
  end
end

