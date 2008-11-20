$:.unshift 'lib'

require 'shinmun'

Dir.chdir(File.dirname(__FILE__))

# use Rack::Reloader

Shinmun::Blog.new.route(self) do

  get '/categories/:category', :format => 'rss' do 
    blog.render_category_feed(params['category'])
  end

  get '/categories/:category' do 
    blog.render_category(params['category'])
  end

  post '/comments' do 
    blog.post_comment(params)
  end

  get '/:year/:month/index' do
    blog.render_month(params['year'].to_i, params['month'].to_i)
  end

  get '/:year/:month/:title' do
    blog.render_post(path)
  end

  get '/index', :format => 'rss' do
    blog.render_index_feed
  end
  
  get '/' do
    blog.render_index_page
  end

  get '.*' do 
    blog.render_page(path)
  end

end

Dir['assets/*'].each do |file|
  map file.sub(/^assets/, '') do
    run Rack::File.new(file)
  end
end
