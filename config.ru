require 'shinmun'
require 'shinmun/controller'
require 'shinmun/admin_controller'

Dir.chdir(File.dirname(__FILE__))

blog = Shinmun::Blog.new

# use Rack::Reloader

map "/#{blog.base_path}" do
  run Shinmun::RackAdapter.new(blog)
end

map '/admin' do
  run Rack::File.new('admin')
end

map "/admin_controller" do
  run Shinmun::AdminController.new(blog)
end

Dir['assets/*'].each do |file|
  map file.sub(/^assets/, '') do
    run Rack::File.new(file)
  end
end
