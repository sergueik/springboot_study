require 'spec_helper'
describe file '/etc/alpine-release' do
  it do
    should be_file
    should be_readable
  end
end
