$:.unshift '../shinmun/lib'

require 'shinmun'

use Rack::Session::Cookie
use Rack::Reloader unless ENV['RACK_ENV'] == 'production'

blog = Shinmun::Blog.new(File.dirname(__FILE__))

blog.config = {
  :author => 'Matthias Georgi',
  :categories => ['Ruby', 'Javascript', 'Actionscript', 'Emacs'],
  :description => 'a crystalline mind in a cloud of code',
  :language => 'en',
  :title => 'Matthias Georgi',
  :url => 'http://www.matthias-georgi.de'
}

run blog
