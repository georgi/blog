require 'shinmun'
require 'shinmun/controller'
require 'shinmun/admin_controller'

blog = Shinmun::Blog.new

# use Rack::Reloader

map "/#{blog.base_path}" do
  run Shinmun::RackAdapter.new(blog)
end

map '/admin' do
  run Rack::File.new('admin')
end

map "/admin_controller" do
  use Rack::CommonLogger
  run Shinmun::AdminController.new(blog)
end

Dir['assets/*'].each do |file|
  map file.sub(/^assets/, '') do
    run Rack::File.new(file)
  end
end
