require 'serverspec'
require 'docker'
require 'basic_test'

tag = ENV['TARGET_HOST']

describe "#{tag}" do
  include Helpers
  
  before(:all) do
    create_image(tag)
  end

  test_node('3.8.2')

  after(:all) do
    delete_image
  end  

end
