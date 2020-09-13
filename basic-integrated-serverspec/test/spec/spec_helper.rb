module Helpers
  def create_image(version, variant='none')
    
    puts 'Building image...'
    @image = Docker::Image.build_from_dir("../#{version}/")
    @image.tag('repo' => 'node', 'tag' => version, 'force' => true)
    
    set :os, :family => 'alpine'
    set :backend, :docker
    set :docker_image, @image.id
    
    puts 'Running tests...'
  end
  
  def delete_image
    puts 'Deleting image...'
    # Stop all containers so we can remove the image
    for container in Docker::Container.all(:all => true)
      container.stop
    end
    
    @image.remove(:force => true)
  end
end

