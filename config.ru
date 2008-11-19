$:.unshift 'lib'

require 'shinmun'

Dir.chdir(File.dirname(__FILE__))

use Rack::Reloader

Shinmun::Blog.new.route(self) do

  get '/categories/:category' do 
    blog.render_category(params['category'])
  end

  post '/comments' do 
    path = params.delete('path')
    preview = params.delete('preview')
    comment = Shinmun::Comment.new(params)

    if preview == 'true'
      blog.render_comments([comment])
    else
      Shinmun::Comment.write(path, comment)
      blog.render_comments(Shinmun::Comment.read(path))
    end
  end

  get '/:year/:month/index' do
    blog.render_month(params['year'].to_i, params['month'].to_i)
  end

  get '' do 
    if path_info == '/'
      blog.render_index_page

    elsif page = blog.find_page(path_info)
      blog.render_page(page)

    elsif post = blog.find_post(path_info)
      blog.render_post(post)

    else
      raise "#{path_info} not found"
    end
  end

end

Dir['assets/*'].each do |file|
  map file.sub(/^assets/, '') do
    run Rack::File.new(file)
  end
end
