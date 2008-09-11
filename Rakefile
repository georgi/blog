$:.unshift "~/workspace/shinmun/lib"

require 'shinmun'
require 'packr'

def compress(filename)
  src = File.read(filename)
  name, ext = filename.split('.')
  min = name + '.min.' + ext
  if !File.exist?(min) or File.mtime(filename) > File.mtime(min)
    open(min, "wb") do |file|
      file << Packr.pack(src)
    end
  end
  File.read(min)
end

task :pack do
  File.open("public/javascripts/all.js", "wb") do |file|
    for script in ['jquery', 'jquery-form', 'prettyDate', 'swfobject']
      file << compress("public/javascripts/#{script}.js") << "\n\n"
    end
  end

  File.open("public/stylesheets/all.css", "wb") do |file|
    for script in [:reset, :grid, :typography, :highlight ]
      file << File.read("public/stylesheets/#{script}.css") << "\n\n"
    end
  end
end

task :render => [:pack] do
  Shinmun::Blog.new.write_all
end

task :clean do
  for comment in Dir["public/controllers/comments/*"]
    puts "deleting #{comment}"
    File.unlink(comment)
  end
end

task :push => [:render, :clean] do
  sh "rsync -avz public/ root@213.239.215.244:/var/www/matthias-georgi.de/"
end

task :default => :render
